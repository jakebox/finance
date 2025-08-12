module Finance.TUI.App
  ( runTUI
  , initialAppState
  ) where

import Brick
import Brick.AttrMap (attrName)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Data.Text as T
import Data.List
import Data.Time (getCurrentTime, showGregorian, utctDay)
import qualified Data.Vector as V
import Data.Ord (Down(..), comparing)
import qualified Graphics.Vty as Vty
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl ((.=))

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Calendar.Month (Month, fromYearMonthValid, addMonths)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Finance.Core
import Finance.ParseBudgetYaml
import Finance.TUI.Types
import Finance.Types
import Finance.Utils (today)
import Text.Printf (printf)

-- Month formatting helper
formatMonth :: Month -> String
formatMonth month = formatTime defaultTimeLocale "%B %Y" month

-- Month navigation helpers
navigateMonth :: Int -> AppState -> AppState
navigateMonth delta st = 
  let newMonth = addMonths (fromIntegral delta) (st ^. currentMonth)
      newComparison = recalculateBudgetComparison st newMonth
  in st & currentMonth .~ newMonth & budgetComparison .~ newComparison

recalculateBudgetComparison :: AppState -> Month -> Maybe (Map.Map Category BudgetComparison)
recalculateBudgetComparison st month = 
  let filteredTxs = filterTransactions (st ^. transactions) (matchesMonthYear month)
      spending = spendingByCategory filteredTxs
  in case Map.lookup month (st ^. budget) of
       Nothing -> Nothing
       Just monthBudget -> Just $ budgetVersusSpending spending monthBudget

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui =
      vBox
        [ hCenter $ str "Finance"
        , str " "
        , vBox
            [ border $ padLeftRight 1 $ drawTransactionSection st
            , str " "
            , border $ padLeftRight 1 $ drawBudgetSection st
            ]
        , str " "
        , drawStatusBar st
        ]

drawTransactionSection :: AppState -> Widget Name
drawTransactionSection st =
  vBox
    [ hCenter $ withAttr (attrName "sectionHeader") $ str "Recent Transactions"
    , str " "
    , drawTransactionList st
    ]

drawBudgetSection :: AppState -> Widget Name
drawBudgetSection st =
  let monthStr = formatMonth (st ^. currentMonth)
      headerText = "Budget - " ++ monthStr
  in vBox
    [ hCenter $ withAttr (attrName "sectionHeader") $ str headerText
    , str " "
    , drawBudgetView st
    ]

drawTransactionList :: AppState -> Widget Name
drawTransactionList st =
  vLimit 10 $
    renderList drawTransaction True (st ^. transactionList)

drawTransaction :: Bool -> Transaction -> Widget Name
drawTransaction selected tx =
  let style = if selected then withAttr (attrName "selected") else id
   in style $
        hBox
          [ padRight (Pad 3) $ str (showGregorian $ txDate tx)
          , hLimit 24 $ padRight Max $ txt (txTitle tx)
          , hLimit 12 $ padLeft Max $ str ("$" ++ show (txAmount tx))
          , padLeft (Pad 8) $ txt (let Category cat = txCategory tx in cat)
          ]

drawBudgetView :: AppState -> Widget Name
drawBudgetView st = case st ^. budgetComparison of
  Nothing -> hCenter $ str "No budget data available for current month"
  Just comparison -> vLimit 8 $ drawBudgetTable comparison

drawBudgetTable :: Map.Map Category BudgetComparison -> Widget Name
drawBudgetTable comparisonMap =
  let rows = map snd $ Map.toList comparisonMap
      sumRow = budgetComparisonSum rows
  in vBox $
    [ hBox
        [ hLimit 15 $ padRight Max $ str "Category"
        , hLimit 10 $ padLeft Max $ str "Budgeted"
        , hLimit 10 $ padLeft Max $ str "Spent"
        , hLimit 11 $ padLeft Max $ str "Remainder"
        , padLeft (Pad 2) $ str "Progress"
        ]
    , hBorder
    ]
      ++ map drawBudgetRow (Map.toList comparisonMap)
     ++ [ hBorder
         , drawSumRow sumRow
         ]
  where
    drawBudgetRow (Category cat, comparison) =
      let budgeted = Finance.Types.budgeted comparison
          actual = Finance.Types.actual comparison
          remainder = Finance.Types.difference comparison
          percentage = if budgeted > 0 then (actual / budgeted) * 100 else 0
          percentageDouble = realToFrac percentage :: Double
          budgetedStr = printf "%.2f" (realToFrac budgeted :: Double)
          actualStr = printf "%.2f" (realToFrac actual :: Double)
          remainderStr = printf "%.2f" (realToFrac remainder :: Double)
          progressBar = drawProgressBar percentageDouble
       in hBox
            [ hLimit 15 $ padRight Max $ txt cat
            , hLimit 10 $ padLeft Max $ str budgetedStr
            , hLimit 10 $ padLeft Max $ str actualStr
            , hLimit 11 $ padLeft Max $ str remainderStr
            , padLeft (Pad 2) $ progressBar
            ]

    drawSumRow sumComparison =
      let budgeted = Finance.Types.budgeted sumComparison
          actual = Finance.Types.actual sumComparison
          remainder = Finance.Types.difference sumComparison
          percentage = if budgeted > 0 then (actual / budgeted) * 100 else 0
          percentageDouble = realToFrac percentage :: Double
          budgetedStr = printf "%.2f" (realToFrac budgeted :: Double)
          actualStr = printf "%.2f" (realToFrac actual :: Double)
          remainderStr = printf "%.2f" (realToFrac remainder :: Double)
          progressBar = drawProgressBar percentageDouble
       in withAttr (attrName "sectionHeader") $ hBox
            [ hLimit 15 $ padRight Max $ str "TOTAL"
            , hLimit 10 $ padLeft Max $ str budgetedStr
            , hLimit 10 $ padLeft Max $ str actualStr
            , hLimit 11 $ padLeft Max $ str remainderStr
            , padLeft (Pad 2) $ progressBar
            ]

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

drawStatusBar :: AppState -> Widget Name
drawStatusBar st =
  case st ^. statusMessage of
    Nothing -> str "Press 'q' to quit, ↑/↓ for transactions, ←/→ for months"
    Just msg -> txt msg

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent e) =
  case e of
    Vty.EvKey (Vty.KChar 'q') [] -> halt
    Vty.EvKey Vty.KEsc [] -> halt
    Vty.EvKey Vty.KLeft [] -> modify (navigateMonth (-1))
    Vty.EvKey Vty.KRight [] -> modify (navigateMonth 1)
    Vty.EvKey (Vty.KChar 'h') [] -> modify (navigateMonth (-1))
    Vty.EvKey (Vty.KChar 'l') [] -> modify (navigateMonth 1)
    _ -> do
      -- All other navigation events go to the transaction list
      zoom transactionList $ handleListEvent e
handleEvent _ = return ()

theApp :: App AppState e Name
theApp =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAttrMap
    }

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    Vty.defAttr
    [ (attrName "selected", Vty.black `on` Vty.white)
    , (attrName "sectionHeader", Vty.withStyle Vty.defAttr Vty.bold)
    ]

initialAppState :: [Transaction] -> IO AppState
initialAppState txs = do
  currentTime <- getCurrentTime
  let currentDay = utctDay currentTime
      (year, month, _) = toGregorian currentDay

  -- Load budget data
  budgetResult <- parseBudgetYaml "budget.yaml"
  let budget = case budgetResult of
        Left _ -> mempty
        Right b -> b

  -- Calculate current month's budget comparison
  currentMonth <- case fromYearMonthValid year month of
    Nothing -> error "Invalid year/month"
    Just m -> return m
  let filteredTxs = filterTransactions txs (matchesMonthYear currentMonth)
      spending = spendingByCategory filteredTxs
      comparison = case Map.lookup currentMonth budget of
        Nothing -> Nothing
        Just monthBudget -> Just $ budgetVersusSpending spending monthBudget

  return $
    AppState
      { _transactions = txs
      , _transactionList = list TransactionList (V.fromList $ sortBy (comparing (Data.Ord.Down . txDate)) txs) 1
      , _budget = budget
      , _budgetComparison = comparison
      , _filters = []
      , _statusMessage = Nothing
      , _currentMonth = currentMonth
      }

runTUI :: [Transaction] -> IO ()
runTUI txs = do
  initialState <- initialAppState txs
  _ <- defaultMain theApp initialState
  return ()
