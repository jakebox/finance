{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Finance.TUI.App (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Finance.Input (readTransactionFile)
import Finance.Types
import Graphics.Vty qualified as Vty

data Name = Transactions | Budget deriving (Show, Ord, Eq)

data St = St
  { _txs :: [Transaction]
  -- , _budget :: BudgetMap
  }

initialState :: [Transaction] -> St
initialState txs = St {..}
  where
    _txs = txs

-- _budget = undefined

drawUI :: St -> [Widget Name]
drawUI st = [ui]
  where
    ui = vBox [hCenter $ str "Finance TUI"]

-- n - resource name type
-- e - event type
appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent (VtyEvent e) =
  case e of
    Vty.EvKey (Vty.KChar 'q') [] -> halt
    _ -> do
      undefined -- zoom
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
