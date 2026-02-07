{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Finance.TUI.App (main) where

import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit (renderEditor)
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Data.Decimal
import Data.Either (fromRight)
import Data.List (sort, sortBy, transpose)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time (addDays, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.Month
import Data.Vector qualified as V
import Finance.Core
  ( budgetComparisonSum
  , budgetVersusSpending
  , categoryMonthBudgetEfficency
  , efficiencyToExplanation
  , filterTransactions
  , matchesMonthYear
  , spendingByCategory
  )
import Finance.Input
import Finance.ParseBudgetYaml
import Finance.PrettyPrint (ppAggregatedSpending)
import Finance.Types (txAmount, txCategory, txDate, txNote, txTitle, unTxTitle, mkTxTitle, TxTitle)
import Finance.Types qualified as Finance
import Finance.Utils qualified as Finance (dayFromS, today)
import Graphics.Vty qualified as Vty
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Lens')
import Text.Parsec
import Text.Printf (printf)

data Name = Transactions | Budget | NameField | DateField | AmountField | CategoryField | NoteField
  deriving (Show, Ord, Eq)

data TransactionInput = FormState
  { _title :: T.Text
  , _date :: Day
  , _amount :: Decimal
  , _category :: Finance.Category
  , _note :: T.Text
  }
makeLenses ''TransactionInput

data SortMode
  = ByDateUp
  | ByDateDown
  | ByAmountUp
  | ByAmountDown
  | ByCategoryUp
  | ByCategoryDown
  | ByTitleUp
  | ByTitleDown
  deriving (Show, Eq)

data St = St
  { _txs :: [Finance.Transaction]
  , _txsList :: List Name Finance.Transaction
  , _txsSort :: SortMode
  , _form :: Form TransactionInput () Name
  , _focus :: FocusRing Name
  , _budgetMonth :: Month
  , _currentBudget :: Maybe (M.Map Finance.Category Finance.BudgetComparison)
  , _budgets :: Finance.BudgetMap
  , _today :: Day
  }

makeLenses ''St

initialState
  :: [Finance.Transaction] -> Finance.BudgetMap -> Day -> St
initialState txs budgets today = St {..}
  where
    _txs = txs
    _txsList = makeTxsList txs
    _txsSort = ByDateDown
    _form = mkForm $ defaultForm today
    _focus = focusRing [Transactions, NameField, DateField, AmountField, CategoryField, NoteField]
    _currentBudget = comparison
    _budgetMonth = month
    _budgets = budgets
    _today = today

    (y, m, _) = toGregorian today
    month = YearMonth y m
    budget = M.lookup month budgets
    comparison = case budget of
      Nothing -> Nothing
      Just b -> Just $ budgetVersusSpending actualSpending b
      where
        actualSpending = (spendingByCategory (filterTransactions txs (matchesMonthYear month)))

defaultForm today =
  FormState
    { _title = ""
    , _date = today
    , _amount = 0.0
    , _category = Finance.categoryFromString ""
    , _note = ""
    }

mkForm :: TransactionInput -> Form TransactionInput () Name
mkForm =
  newForm
    [ (str "Title:    " <+>) @@= titleField
    , (str "Date:     " <+>) @@= dateField
    , (str "Amount:   " <+>) @@= amtField
    , (str "Category: " <+>) @@= categoryField
    , (str "Note:     " <+>) @@= editTextField note NoteField (Just 1)
    ]
  where
    -- titleField = editField title NameField (Just 1) toText validate render id
    --   where
    --     toText t = unTxTitle t
    --     validate [t] = case mkTxTitle t of
    --                      Left err -> Nothing
    --                      Right t -> Just t
    --     render [t] = txt t
    titleField = editField title NameField (Just 1) id validate render id
      where
        render [t] = txt t
        validate [t] = case mkTxTitle t of
                         Left _  -> Nothing -- Brick marks field invalid
                         Right _ -> Just t  -- Returns the text if valid
        validate _   = Nothing

    amtField = editField amount AmountField (Just 1) toText validate render id
      where
        toText d = T.pack $ show d
        validate [t] = Just (realFracToDecimal 2 (read $ T.unpack t))
        render [t] = txt t

    dateField = editField date DateField (Just 1) toText validate render id
      where
        toText d = T.pack $ show d
        validate [t] = Finance.dayFromS (T.unpack t)
        render [t] = txt t

    categoryField = editField category CategoryField (Just 1) toText validate render id
      where
        toText = Finance.getCategoryText
        validate [t] = Just $ Finance.categoryFromString $ T.unpack t
        render [t] = txt t

drawUI :: St -> [Widget Name]
drawUI st = [ui]
  where
    ui =
      vBox
        [ padBottom (Pad 1) statusBar
        , hBox
            [ transactions
            , padLeft (Pad 1) trendsChart
            ]
        , hBox
            [ budget
            , padLeft (Pad 1) summary
            ]
        , inputForm
        , infoLine
        ]

    budget =
      overrideAttr borderAttr orangeBorderAttr
        $ borderWithLabel (withAttr headingAttr $ str $ "Budget for " <> show (st ^. budgetMonth))
          . padLeftRight 1
        $ hLimit 75 (vLimit 11 budgetDisplay)
    budgetDisplay = drawBudgetTable (st ^. today) (st ^. budgetMonth) (st ^. currentBudget)
    statusBar = drawStatusBar st
    infoLine = withAttr statusBarAttr $ str "^q: quit │ n: new │ ^l: list │ s/a/c/t: sort │ ←/→: budget month"
    inputForm =
      overrideAttr borderAttr orangeBorderAttr $
        borderWithLabel (withAttr headingAttr $ str "Add a new transaction [n]") . padLeftRight 1 $
          hLimit 50 (renderForm (st ^. form))
    transactions =
      overrideAttr borderAttr orangeBorderAttr $
        borderWithLabel (withAttr headingAttr $ str "Recent Transactions") . padLeftRight 1 $
          hLimit 75 $
            vBox
              [ transactionHeader
              , hBorder
              , vLimit 13 transactionList
              ]
    transactionHeader =
      withAttr boldAttr $
        hBox
          [ padRight (Pad 3) $ hLimit 10 $ padRight Max $ str "Date"
          , hLimit 20 $ padRight Max $ str "Description"
          , hLimit 12 $ padLeft Max $ str "Amount"
          , padLeft (Pad 5) $ str "Category"
          ]
    transactionList = renderList renderTx True (st ^. txsList)
    renderTx selected tx =
      style $
        hBox
          [ padRight (Pad 3) $ str (show $ txDate tx)
          , hLimit 20 $ padRight Max $ txt (unTxTitle $ txTitle tx)
          , hLimit 12 $ padLeft Max $ str ("$" ++ show (txAmount tx))
          , padLeft (Pad 5) $ txt (let Finance.Category cat = txCategory tx in cat)
          ]
      where
        style = if selected then withAttr (attrName "selected") else id
    summary =
      overrideAttr borderAttr orangeBorderAttr $
        borderWithLabel (withAttr headingAttr $ str "Summary") . padLeftRight 1 $
          summaryDetails (st ^. txs)

    trendsChart =
      overrideAttr borderAttr orangeBorderAttr $
        borderWithLabel (withAttr headingAttr $ str "7-Day Spending Trend") . padLeftRight 1 $
          hLimit 30 $
            drawSpendingChart (st ^. txs) (st ^. today)

drawSpendingChart :: [Finance.Transaction] -> Day -> Widget Name
drawSpendingChart txs today =
  let
    -- Get last 7 days of spending
    days = [addDays (-i) today | i <- [6, 5, 4, 3, 2, 1, 0]]
    dailySpending = map (getDaySpending txs) days
    maxSpending = if null dailySpending then 1 else maximum dailySpending

    -- Create chart bars (height 8)
    chartHeight = 8
    bars = map (createBar chartHeight maxSpending) dailySpending

    -- Day labels (show last 2 chars: Mo, Tu, We, etc.)
    dayLabels = map (take 2 . formatTime defaultTimeLocale "%a") days
   in
    vBox
      [ -- Chart bars from top to bottom
        vBox $ map hBox $ transpose $ map (map (hLimit 3 . hCenter . str) . Prelude.reverse) bars
      , hBorder
      , -- Day labels
        hBox $ map (\label -> hLimit 3 $ hCenter $ str label) dayLabels
      , hBorder
      , -- Values
        hBox $ map (\amount -> hLimit 3 $ hCenter $ str $ show (truncate amount :: Int)) dailySpending
      ]
  where
    getDaySpending :: [Finance.Transaction] -> Day -> Decimal
    getDaySpending transactions day =
      sum [txAmount tx | tx <- transactions, txDate tx == day]

    createBar :: Int -> Decimal -> Decimal -> [String]
    createBar height maxVal val =
      let barHeight = if maxVal > 0 then round (realToFrac val * fromIntegral height / realToFrac maxVal) else 0
          filledChars = replicate barHeight "█"
          emptyChars = replicate (height - barHeight) " "
       in filledChars ++ emptyChars

drawStatusBar :: St -> Widget Name
drawStatusBar st =
  let currentFocus = case focusGetCurrent (st ^. focus) of
        Just Transactions -> "List Mode"
        Just NameField -> "Form Mode"
        Just DateField -> "Form Mode"
        Just AmountField -> "Form Mode"
        Just CategoryField -> "Form Mode"
        Just NoteField -> "Form Mode"
        _ -> "View Mode"

      selectedInfo = case listSelected (st ^. txsList) of
        Nothing -> "No selection"
        Just idx -> printf "Transaction %d of %d" (idx + 1) (length (st ^. txs))

      totalAmount = sum $ map txAmount (st ^. txs)
      totalInfo = printf "Total: $%.2f" (realToFrac totalAmount :: Double)

      sortInfo = case st ^. txsSort of
        ByDateDown -> "Sort: Date ↓"
        ByDateUp -> "Sort: Date ↑"
        ByAmountDown -> "Sort: Amount ↓"
        ByAmountUp -> "Sort: Amount ↑"
        ByCategoryDown -> "Sort: Category ↓"
        ByCategoryUp -> "Sort: Category ↑"
        ByTitleDown -> "Sort: Title ↓"
        ByTitleUp -> "Sort: Title ↑"

      efficiency = case st ^. currentBudget of
        Just comparisonMap ->
          let eff = categoryMonthBudgetEfficency (st ^. today) (st ^. budgetMonth) comparisonMap
           in printf "Efficiency: %+.1f%%" (eff * 100)
        Nothing -> "No Budget"

      leftSection = str $ "Finance TUI │ [" <> currentFocus <> "]"
      centerSection = hBox [str $ selectedInfo <> " │ ", withAttr boldAttr $ str totalInfo]
      rightSection = str $ sortInfo <> " │ " <> efficiency
   in withAttr statusBarAttr $
        hBox
          [ leftSection
          , padLeft Max centerSection
          , padLeft Max rightSection
          ]

summaryDetails :: [Finance.Transaction] -> Widget Name
summaryDetails txs =
  vBox
    [ withAttr boldAttr $ drawLine "Category" "Amount"
    , vBox (M.foldrWithKey acc [] aggSpending)
    , withAttr boldAttr $ drawLine "Total" (show total)
    ]
  where
    aggSpending = spendingByCategory txs
    total = M.foldr (+) 0 aggSpending
    acc cat amt acc_widgets =
      let newWidget = drawLine (T.unpack $ Finance.getCategoryText cat <> ": ") (show amt)
       in newWidget : acc_widgets
    drawLine :: String -> String -> Widget Name
    drawLine label value =
      hBox
        [ hLimit 15 $ padRight Max $ str label
        , hLimit 9 $ padLeft Max $ str value
        ]

drawBudgetTable
  :: Day -> Month -> Maybe (M.Map Finance.Category Finance.BudgetComparison) -> Widget Name
drawBudgetTable today budgetMonth maybeComparisonMap =
  case maybeComparisonMap of
    Just comparisonMap ->
      let rows = map snd $ M.toList comparisonMap
          sumRow = (Finance.categoryFromString "TOTAL", budgetComparisonSum rows)
          efficiency = categoryMonthBudgetEfficency today budgetMonth comparisonMap
          efficiencyStr = printf "Overall efficiency: %+.1f%% (%s)" (efficiency * 100) (efficiencyToExplanation efficiency)
          header =
            vBox
              [ oneLine "Category" "Budgeted" "Spent" "Remainder" (str "Progress")
              , hLimit 71 hBorder
              ]
          categoryRows = vBox $ map drawBudgetRow (M.toList comparisonMap)
          totalSection =
            vBox
              [ hLimit 71 hBorder
              , drawBudgetRow sumRow
              , padTop (Pad 1) $ hCenter $ withAttr italicAttr $ str efficiencyStr
              ]
       in vBox [header, padBottom Max categoryRows, totalSection]
    Nothing -> hLimit 75 $ hCenter (str "No budget found for this month.") <+> fill ' '
  where
    oneLine a b c d e =
      hBox
        [ hLimit 15 $ padRight Max $ str a
        , hLimit 10 $ padLeft Max $ str b
        , hLimit 10 $ padLeft Max $ str c
        , hLimit 12 $ padLeft Max $ str d
        , padLeft (Pad 2) e
        ]

    drawBudgetRow (Finance.Category cat, comparison) =
      let budgeted = Finance.budgeted comparison
          actual = Finance.actual comparison
          remainder = Finance.difference comparison
          percentage = if budgeted > 0 then (actual / budgeted) * 100 else 0
          percentageDouble = realToFrac percentage :: Double
          budgetedStr = printf "%.2f" (realToFrac budgeted :: Double)
          actualStr = printf "%.2f" (realToFrac actual :: Double)
          remainderStr = printf "%.2f" (realToFrac remainder :: Double)
          progressBar = drawProgressBar percentageDouble
       in oneLine (T.unpack cat) budgetedStr actualStr remainderStr progressBar

drawProgressBar :: Double -> Widget Name
drawProgressBar percentage =
  let barWidth = 15
      filledWidth = max 0 $ min barWidth $ round (percentage * fromIntegral barWidth / 100)
      emptyWidth = barWidth - filledWidth
      filled = replicate filledWidth '█'
      empty = replicate emptyWidth '░'
      barText = filled ++ empty
      percentText = printf "%6.1f%%" percentage
      fullText = barText ++ percentText
   in str fullText

sortTransactionList :: SortMode -> List Name Finance.Transaction -> List Name Finance.Transaction
sortTransactionList mode = listElementsL %~ (V.fromList . sortBy cmp . V.toList)
  where
    cmp :: Finance.Transaction -> Finance.Transaction -> Ordering
    cmp a b = case mode of
      ByDateDown -> compare b.txDate a.txDate
      ByDateUp -> compare a.txDate b.txDate
      ByAmountDown -> compare b.txAmount a.txAmount
      ByAmountUp -> compare a.txAmount b.txAmount
      ByCategoryDown -> compare b.txCategory a.txCategory
      ByCategoryUp -> compare a.txCategory b.txCategory
      ByTitleDown -> compare b.txTitle a.txTitle
      ByTitleUp -> compare a.txTitle b.txTitle

makeTxsList :: [Finance.Transaction] -> List Name Finance.Transaction
makeTxsList txs = sortTransactionList ByDateDown $ list Transactions (V.fromList txs) 1

-- n - resource name type
-- e - event type
appEvent :: BrickEvent Name () -> EventM Name St ()
appEvent ev@(VtyEvent vtyEv) =
  case vtyEv of
    Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] -> modify (focus %~ focusSetCurrent Transactions)
    Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> halt
    Vty.EvKey Vty.KEsc [] -> halt
    _ -> do
      st <- get
      case focusGetCurrent (st ^. focus) of
        Just NameField -> case vtyEv of
          Vty.EvKey Vty.KEnter [] -> do
            let currentForm = st ^. form
            if allFieldsValid currentForm
              then do
                newTx <- liftIO $ createTransaction currentForm
                let newTxs = (st ^. txs) <> [newTx]
                    txsList' = makeTxsList newTxs
                put $ st {_form = mkForm (defaultForm (st ^. today)), _txs = newTxs, _txsList = txsList'}
               else
                continueWithoutRedraw
          _ -> zoom form $ handleFormEvent ev
        _ -> case vtyEv of
          Vty.EvKey Vty.KLeft [] -> updateBudget (-1)
          Vty.EvKey Vty.KRight [] -> updateBudget 1
          Vty.EvKey (Vty.KChar 'n') [] -> modify (focus %~ focusSetCurrent NameField)
          Vty.EvKey (Vty.KChar 's') [] -> toggleSort ByDateUp ByDateDown
          Vty.EvKey (Vty.KChar 'a') [] -> toggleSort ByAmountUp ByAmountDown
          Vty.EvKey (Vty.KChar 'c') [] -> toggleSort ByCategoryUp ByCategoryDown
          Vty.EvKey (Vty.KChar 't') [] -> toggleSort ByTitleUp ByTitleDown
          _ -> zoom txsList $ handleListEventVi handleListEvent vtyEv
  where
    toggleSort :: SortMode -> SortMode -> EventM Name St ()
    toggleSort upMode downMode = do
      st <- get
      let newMode = case st ^. txsSort of
            mode | mode == downMode -> upMode
            _ -> downMode
      modify (txsSort .~ newMode)
      let newList = sortTransactionList newMode (st ^. txsList)
      modify (txsList .~ newList)

    updateBudget :: Integer -> EventM Name St ()
    updateBudget dir = do
      -- switch budget to the next month
      st <- get
      let newMonth = addMonths dir (st ^. budgetMonth)
      case M.lookup newMonth (st ^. budgets) of
        Just budget -> do
          let comparison =
                Just $
                  budgetVersusSpending
                    (spendingByCategory (filterTransactions (st ^. txs) (matchesMonthYear newMonth)))
                    budget
          modify (currentBudget .~ comparison)
          modify (budgetMonth .~ newMonth)
        Nothing -> do
          modify (currentBudget .~ Nothing)
          modify (budgetMonth .~ newMonth)
appEvent _ = pure ()

createTransaction :: Form TransactionInput () Name -> IO Finance.Transaction
createTransaction f = do
  addTxToTransactionFile "transactions.csv" tx
  pure tx
  where
    values = formState f
    tx =
      Finance.Transaction
        { txTitle = fromRight (error "Validated") $ mkTxTitle (values ^. title)
        , txDate = values ^. date
        , txAmount = values ^. amount
        , txCategory = values ^. category
        , txNote = values ^. note
        }

headingAttr :: AttrName
headingAttr = attrName "headingAttr"

boldAttr :: AttrName
boldAttr = attrName "bold"

statusBarAttr :: AttrName
statusBarAttr = attrName "statusbar"

orangeBorderAttr :: AttrName
orangeBorderAttr = attrName "orangeBorder"

italicAttr :: AttrName
italicAttr = attrName "italic"

finance :: App St () Name
finance = App {..}
  where
    appAttrMap =
      const $
        attrMap
          Vty.defAttr
          [ (attrName "selected", Vty.black `on` Vty.white)
          , (headingAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
          , (invalidFormInputAttr, Vty.white `on` Vty.red)
          , (E.editAttr, Vty.defAttr `Vty.withStyle` Vty.underline)
          , (boldAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
          , (statusBarAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
          ,
            ( orangeBorderAttr
            , Vty.defAttr `Vty.withForeColor` Vty.rgbColor (255 :: Int) (165 :: Int) (0 :: Int)
            )
          , (italicAttr, Vty.defAttr `Vty.withStyle` Vty.italic)
          ]
    appDraw = drawUI
    appHandleEvent = appEvent
    appStartEvent = pure ()
    appChooseCursor st cs = do
      case focusGetCurrent (st ^. focus) of
        Just Transactions -> showCursorNamed Transactions cs
        Just n -> case focusGetCurrent (formFocus (st ^. form)) of
          Just field -> showCursorNamed field cs
          Nothing -> Nothing
        Nothing -> Nothing

main :: IO ()
main = do
  txs <- readTransactionFile "transactions.csv"
  today <- Finance.today
  budgets <-
    parseBudgetYaml "budget.yaml" >>= \case
      Left err -> error $ show err
      Right bud -> return bud

  let app = finance
      st = initialState txs budgets today
  finalState <- defaultMain app st
  return ()
