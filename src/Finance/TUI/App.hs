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
import Data.List (sort, sortBy)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.Month
import Data.Vector qualified as V
import Finance.Core
  ( budgetComparisonSum
  , budgetVersusSpending
  , filterTransactions
  , matchesMonthYear
  , spendingByCategory
  )
import Finance.Input
import Finance.ParseBudgetYaml
import Finance.PrettyPrint (ppAggregatedSpending)
import Finance.Types (txAmount, txCategory, txDate, txNote, txTitle)
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

data SortMode = ByDateUp | ByDateDown | ByAmountUp | ByAmountDown

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
    _currentBudget = Just comparison
    _budgetMonth = month
    _budgets = budgets
    _today = today

    (y, m, _) = toGregorian today
    month = YearMonth y m
    budget = fromJust $ M.lookup month budgets
    comparison =
      budgetVersusSpending (spendingByCategory (filterTransactions txs (matchesMonthYear month))) budget

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
    [ (str "Title:    " <+>) @@= editTextField title NameField (Just 1)
    , (str "Date:     " <+>) @@= dateField
    , (str "Amount:   " <+>) @@= amtField
    , (str "Category: " <+>) @@= categoryField
    , (str "Note:     " <+>) @@= editTextField note NoteField (Just 1)
    ]
  where
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
        [ padBottom (Pad 1) $ hCenter $ str "Finance TUI"
        , transactions
        , hBox
            [ budget
            , padTopBottom 1 . padLeft (Pad 1) $ summary
            ]
        , inputForm
        , infoLine
        ]

    budget = borderWithLabel (withAttr headingAttr $ str $ "Budget for " <> show (st ^. budgetMonth)) . padLeftRight 1 $ budgetDisplay
    budgetDisplay = drawBudgetTable (st ^. currentBudget)
    infoLine = str "Press ctrl + : 'q' to quit, 'n' for new item, 'l' for list"
    inputForm = borderWithLabel (withAttr headingAttr $ str "Add a new transaction") . padLeftRight 1 $ hLimit 50 (renderForm (st ^. form))
    transactions = borderWithLabel (withAttr headingAttr $ str "Recent Transactions") . padLeftRight 1 $ vLimit 15 transactionList
    transactionList = renderList renderTx True (st ^. txsList)
    renderTx selected tx =
      style $
        hBox
          [ padRight (Pad 3) $ str (show $ txDate tx)
          , hLimit 24 $ padRight Max $ txt (txTitle tx)
          , hLimit 12 $ padLeft Max $ str ("$" ++ show (txAmount tx))
          , padLeft (Pad 8) $ txt (let Finance.Category cat = txCategory tx in cat)
          ]
      where
        style = if selected then withAttr (attrName "selected") else id
    summary = borderWithLabel (withAttr headingAttr $ str "Summary") . padLeftRight 1 $ summaryDetails (st ^. txs)

summaryDetails :: [Finance.Transaction] -> Widget Name
summaryDetails txs =
  vBox
    [ withAttr boldAttr $ drawLine "Category" "Amount"
    , vBox (M.foldrWithKey acc [] aggSpending)
    , drawLine "Total" (show total)
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

drawBudgetTable :: Maybe (M.Map Finance.Category Finance.BudgetComparison) -> Widget Name
drawBudgetTable maybeComparisonMap =
  case maybeComparisonMap of
    Just comparisonMap ->
      let rows = map snd $ M.toList comparisonMap
          sumRow = (Finance.categoryFromString "TOTAL", budgetComparisonSum rows)
       in vBox $
            [ oneLine "Category" "Budgeted" "Spent" "Remainder" (str "Progress")
            , hLimit 68 hBorder
            ]
              ++ map drawBudgetRow (M.toList comparisonMap)
              ++ [ hLimit 68 hBorder
                 , drawBudgetRow sumRow
                 ]
    Nothing -> vBox [hCenter $ str "No budget found for this month."]
  where
    oneLine a b c d e =
      hBox
        [ hLimit 15 $ padRight Max $ str a
        , hLimit 10 $ padLeft Max $ str b
        , hLimit 10 $ padLeft Max $ str c
        , hLimit 11 $ padLeft Max $ str d
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
      percentText = printf " %.1f%%" percentage
      fullText = barText ++ percentText
   in str fullText

sortListByDate :: SortMode -> List Name Finance.Transaction -> List Name Finance.Transaction
sortListByDate mode = listElementsL %~ (V.fromList . sortBy cmp . V.toList)
  where
    cmp :: Finance.Transaction -> Finance.Transaction -> Ordering
    cmp a b = case mode of
      ByDateDown -> compare b.txDate a.txDate
      ByDateUp -> compare a.txDate b.txDate

makeTxsList :: [Finance.Transaction] -> List Name Finance.Transaction
makeTxsList txs = sortListByDate ByDateDown $ list Transactions (V.fromList txs) 1

-- n - resource name type
-- e - event type
appEvent :: BrickEvent Name () -> EventM Name St ()
appEvent ev@(VtyEvent vtyEv) =
  case vtyEv of
    Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] -> modify (focus %~ focusSetCurrent Transactions)
    Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] -> modify (focus %~ focusSetCurrent NameField)
    Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> halt
    Vty.EvKey Vty.KEsc [] -> halt
    _ -> do
      st <- get
      case focusGetCurrent (st ^. focus) of
        Just NameField -> case vtyEv of
          Vty.EvKey Vty.KEnter [] -> do
            let currentForm = st ^. form
            newTx <- liftIO $ createTransaction currentForm
            let newTxs = (st ^. txs) <> [newTx]
                txsList' = makeTxsList newTxs
            put $ st {_form = mkForm (defaultForm (st ^. today)), _txs = newTxs, _txsList = txsList'}
          _ -> zoom form $ handleFormEvent ev
        _ -> case vtyEv of
          Vty.EvKey Vty.KLeft [] -> updateBudget (-1)
          Vty.EvKey Vty.KRight [] -> updateBudget 1
          Vty.EvKey (Vty.KChar 's') [] -> do
            st <- get
            let newMode = case st ^. txsSort of
                  ByDateUp -> ByDateDown
                  ByDateDown -> ByDateUp
            modify (txsSort .~ newMode)
            let newList = sortListByDate newMode (st ^. txsList)
            modify (txsList .~ newList)
          _ -> zoom txsList $ handleListEventVi handleListEvent vtyEv
  where
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
        { txTitle = values ^. title
        , txDate = values ^. date
        , txAmount = values ^. amount
        , txCategory = values ^. category
        , txNote = values ^. note
        }

headingAttr :: AttrName
headingAttr = attrName "headingAttr"

boldAttr :: AttrName
boldAttr = attrName "bold"

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
