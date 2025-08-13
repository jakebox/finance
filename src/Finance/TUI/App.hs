{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Finance.TUI.App (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.List
import Data.Vector qualified as V
import Finance.Input
import Finance.Types
import Graphics.Vty qualified as Vty
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Data.List (sort)

data Name = Transactions | Budget deriving (Show, Ord, Eq)

data St = St
  { _txs :: [Transaction]
  , _txsList :: List Name Transaction
  -- , _budget :: BudgetMap
  }

makeLenses ''St

initialState :: [Transaction] -> St
initialState txs = St {..}
  where
    _txs = txs
    _txsList = list Transactions (V.fromList txs) 1

-- _budget = undefined

drawUI :: St -> [Widget Name]
drawUI st = [ui]
  where
    ui =
      vBox
        [ hCenter $ str "Finance TUI"
        , str " "
        , transactions
        ]

    transactions = vBox [border $ padLeftRight 1 transactionList]
    transactionList = renderList renderTx True (st ^. txsList)
    renderTx :: Bool -> Transaction -> Widget Name
    renderTx selected tx =
      style $
        hBox
          [ padRight (Pad 3) $ str (show $ txDate tx)
          , hLimit 24 $ padRight Max $ txt (txTitle tx)
          , hLimit 12 $ padLeft Max $ str ("$" ++ show (txAmount tx))
          , padLeft (Pad 8) $ txt (let Category cat = txCategory tx in cat)
          ]
      where
        style = if selected then withAttr (attrName "selected") else id

-- n - resource name type
-- e - event type
appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent (VtyEvent e) =
  case e of
    Vty.EvKey (Vty.KChar 'q') [] -> halt
    Vty.EvKey Vty.KEsc [] -> halt
    ev -> zoom txsList $ handleListEventVi handleListEvent ev
appEvent _ = pure ()

finance :: App St e Name
finance = App {..}
  where
    appAttrMap = const $ attrMap Vty.defAttr [(attrName "selected", Vty.black `on` Vty.white)]
    appChooseCursor = neverShowCursor
    appDraw = drawUI
    appHandleEvent = appEvent
    appStartEvent = pure ()

main :: IO ()
main = do
  txs <- readTransactionFile "transactions.csv"
  let app = finance
      st = initialState txs
  finalState <- defaultMain app st
  return ()
