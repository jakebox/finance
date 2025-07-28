{-# OPTIONS_GHC -Wno-orphans #-}

module Finance.Input (readTransactionFile, stringToFilters, stringToYearMonth, runAddTx, addTxToTransactionFile) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Decimal
import Data.Time
import Data.Time.Calendar (MonthOfYear, Year)
import Data.Time.Calendar.Month
import qualified Data.Vector as V

import Finance.Core
import Finance.Types
import Finance.Utils

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative (command)
import Text.Parsec
import qualified Text.Parsec.String as P (Parser)

instance FromField Day where
  parseField :: Field -> Parser Day
  parseField s = do
    d <- parseField s
    case dayFromS d of
      Just date -> pure date
      Nothing -> error "Bad parse date"

instance ToField Day where
  toField day = toField (formatTime defaultTimeLocale "%F" day)

instance FromField Category where
  parseField :: Field -> Parser Category
  parseField s = Category <$> parseField s

instance ToField Category where
  toField = pack . T.unpack . getCategoryText

instance FromField Decimal where
  parseField :: Field -> Parser Decimal
  parseField d = do
    doubleVal :: Float <- parseField d
    pure $ realFracToDecimal 2 doubleVal

instance ToField Decimal where
    toField = pack . show

instance FromNamedRecord Transaction where
  parseNamedRecord :: NamedRecord -> Parser Transaction
  parseNamedRecord m =
    Transaction
      <$> m .: "Title"
      <*> m .: "Date"
      <*> m .: "Amount"
      <*> m .: "Category"
      <*> m .: "Note"

instance ToNamedRecord Transaction where
  toNamedRecord (Transaction txTitle txDate txAmount txCategory txNote) =
    namedRecord
      [ "Title" .= txTitle
      , "Date" .= txDate
      , "Amount" .= txAmount
      , "Category" .= txCategory
      , "Note" .= txNote
      ]

transactionsHeader :: Header
transactionsHeader = V.fromList ["Title", "Date", "Amount", "Category", "Note"]

readTransactionFile :: FilePath -> IO [Transaction]
readTransactionFile fp = do
  csvData <- BS.readFile fp
  case decodeByName csvData of
    Left err -> error err
    Right (_, txs :: V.Vector Transaction) -> pure $ V.toList txs

addTxToTransactionFile :: FilePath -> Transaction -> IO ()
addTxToTransactionFile fp tx = BS.appendFile fp csvRow
  where
    row = toNamedRecord tx
    csvRow = encodeByNameWith opts transactionsHeader [row]
    opts =
      defaultEncodeOptions
        { encIncludeHeader = False
        }

runAddTx :: ExceptT ParseError IO Transaction
runAddTx = do
  liftIO $ T.putStr "Name: "
  title <- liftIO T.getLine

  liftIO $ T.putStr "Date: "
  dateText <- liftIO getLine
  date <- ExceptT . return $ parse parseDay "input" dateText

  liftIO $ T.putStr "Amount: "
  amountText <- liftIO getLine
  amount <- ExceptT . return $ parse parseAmount "input" amountText
  liftIO $ T.putStr "Category: "
  categoryText <- liftIO getLine
  liftIO $ T.putStr "Note: "
  note <- liftIO T.getLine

  return $
    Transaction
      { txTitle= title
      , txDate = date
      , txAmount = amount
      , txCategory = categoryFromString categoryText
      , txNote = note
      }

-- --- Parsers

stringToFilters :: Maybe String -> TransactionFilterP
stringToFilters s = case s of
  Just s -> case parse parseAnyFilter "input" s of
    Left err -> identityFilter
    Right fs -> combinePredicateFilters fs
  Nothing -> identityFilter

parseAnyFilter :: P.Parser [TransactionFilterP]
parseAnyFilter =
  many1
    ( try (spaces >> parseAmountFilter)
        <|> try (spaces >> parseMonthFilter)
        <|> try (spaces >> parseTitleFilter)
    )

parseAmountFilter :: P.Parser TransactionFilterP
parseAmountFilter = amountEquals <$> parseAmount

parseMonthFilter :: P.Parser TransactionFilterP
parseMonthFilter = matchesMonth <$> parseMonth

parseTitleFilter :: P.Parser TransactionFilterP
parseTitleFilter = titleInfix <$> parseTitle

-- Parsing a year/month or month for the budget command

stringToYearMonth :: String -> Month
stringToYearMonth s = case parse parseYearMonth "input" s of
  Left err -> error $ show err
  Right m -> m

parseYearMonth :: P.Parser Month
parseYearMonth = do
  year <- parseYear
  _ <- char '-'
  month <- parseMonth
  case fromYearMonthValid year month of
    Just m -> pure m
    Nothing -> error "bad parse"

parseDay :: P.Parser Day
parseDay = do
  dayStr <- many1 (noneOf " \n")
  case dayFromS dayStr of
    Just date -> pure date
    Nothing -> error "Bad parse date"


----------

parseAmount :: P.Parser Decimal
parseAmount = do
  _ <- char '$'
  whole <- many1 digit
  frac <- option "" ((:) <$> char '.' <*> many1 digit)
  let fullNumStr = whole ++ frac
  pure $ realFracToDecimal 2 (read fullNumStr)

parseYear :: P.Parser Year
parseYear = do
  yearStr <- many1 digit
  let yearInt :: Integer = read yearStr
  pure yearInt

parseMonth :: P.Parser MonthOfYear
parseMonth = do
  monthStr <- many1 (noneOf " \n")
  case arbitraryMonToMonth monthStr of
    Just m -> pure m
    Nothing -> fail "Not a month"

parseTitle :: P.Parser T.Text
parseTitle = T.pack <$> many1 (noneOf " \n")

arbitraryMonToMonth :: String -> Maybe MonthOfYear
arbitraryMonToMonth s
  | s == "jan" || s == "january" || s == "01" || s == "1" = Just 1
  | s == "feb" || s == "february" || s == "02" || s == "2" = Just 2
  | s == "mar" || s == "march" || s == "03" || s == "3" = Just 3
  | s == "apr" || s == "april" || s == "04" || s == "4" = Just 4
  | s == "may" || s == "05" || s == "5" = Just 5
  | s == "jun" || s == "june" || s == "06" || s == "6" = Just 6
  | s == "jul" || s == "july" || s == "07" || s == "7" = Just 7
  | s == "aug" || s == "august" || s == "08" || s == "8" = Just 8
  | s == "sep" || s == "september" || s == "09" || s == "9" = Just 9
  | s == "oct" || s == "october" || s == "10" || s == "10" = Just 10
  | s == "nov" || s == "november" || s == "11" || s == "11" = Just 11
  | s == "dec" || s == "december" || s == "12" || s == "12" = Just 12
  | otherwise = Nothing
