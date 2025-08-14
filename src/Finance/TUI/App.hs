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
import Data.List (sort)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Vector qualified as V
import Finance.Input
import Finance.Types (txAmount, txCategory, txDate, txNote, txTitle)
import Finance.Types qualified as Finance
import Finance.Utils (dayFromS)
import Graphics.Vty qualified as Vty
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Lens')
import Text.Parsec

data Name = Transactions | Budget | NameField | DateField | AmountField | NoteField
  deriving (Show, Ord, Eq)

data TransactionInput = FormState
  { _title :: T.Text
  , _date :: Day
  , _amount :: Decimal
  , _category :: T.Text
  , _note :: T.Text
  }
makeLenses ''TransactionInput

data St = St
  { _txs :: [Finance.Transaction]
  , _txsList :: List Name Finance.Transaction
  , _form :: Form TransactionInput () Name
  , _focus :: FocusRing Name
  }

makeLenses ''St

defaultForm =
  FormState {_title = "", _date = fromGregorian 2000 1 1, _amount = 0.0, _category = "", _note = ""}

mkForm :: TransactionInput -> Form TransactionInput () Name
mkForm =
  newForm
    [ (str "Title:  " <+>) @@= editTextField title NameField (Just 1)
    , (str "Date:   " <+>) @@= dateField
    , (str "Amount: " <+>) @@= amtField
    , (str "Note:   " <+>) @@= editTextField note NoteField (Just 1)
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
        validate [t] = dayFromS (T.unpack t)
        render [t] = txt t

initialState :: [Finance.Transaction] -> St
initialState txs = St {..}
  where
    _txs = txs
    _txsList = list Transactions (V.fromList txs) 1
    _form = mkForm defaultForm
    _focus = focusRing [Transactions, NameField, DateField, AmountField, NoteField]

drawUI :: St -> [Widget Name]
drawUI st = [ui]
  where
    ui =
      vBox
        [ hCenter $ str "Finance TUI"
        , str " "
        , transactions
        , inputForm
        , infoLine
        ]

    infoLine = str "Press ctrl + : 'q' to quit, 'n' for new item, 'l' for list"
    inputForm = vBox [border $ padLeftRight 1 (str "Add a new transaction" <=> renderForm (st ^. form))]
    transactions = vBox [border $ padLeftRight 1 transactionList]
    transactionList = renderList renderTx True (st ^. txsList)
    renderTx :: Bool -> Finance.Transaction -> Widget Name
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
        Just Transactions -> zoom txsList $ handleListEventVi handleListEvent vtyEv
        Just NameField -> case vtyEv of
          Vty.EvKey Vty.KEnter [] -> do
            let currentForm = st ^. form
            liftIO $ createTransaction currentForm
            put $ st {_form = mkForm defaultForm}
          _ -> zoom form $ handleFormEvent ev
appEvent _ = pure ()

createTransaction :: Form TransactionInput () Name -> IO ()
createTransaction f = addTxToTransactionFile "transactions.csv" tx
  where
    values = formState f
    tx =
      Finance.Transaction
        { txTitle = values ^. title
        , txDate = values ^. date
        , txAmount = values ^. amount
        , txCategory = Finance.categoryFromString $ T.unpack (values ^. category)
        , txNote = values ^. note
        }

finance :: App St () Name
finance = App {..}
  where
    appAttrMap =
      const $
        attrMap
          Vty.defAttr
          [ (attrName "selected", Vty.black `on` Vty.white)
          , -- , (focusedFormInputAttr, Vty.black `on` Vty.yellow)
            (invalidFormInputAttr, Vty.white `on` Vty.red)
          , (E.editAttr, Vty.white `on` Vty.black)
          -- , (E.editFocusedAttr, Vty.black `on` Vty.yellow)
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
  let app = finance
      st = initialState txs
  finalState <- defaultMain app st
  return ()
