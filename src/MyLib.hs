{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MyLib (parseAndPrint) where

import Data.List
import Data.Map.Strict (Map, insertWith)
import Data.Text (Text, pack)
import Data.Time (Day, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Numeric (readFloat, readSigned)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map.Strict as M

-- Internal types
data Transaction = Transaction
  { txDate :: Day
  , txTitle :: Text
  , txDescription :: Text
  , txCategory :: Category
  , txAmount :: Double
  }
  deriving (Show)

newtype Category = CategoryName Text
  deriving (Eq, Ord, Show)

-- File types

data TransactionBlock = TransactionBlock
  { txHeader :: TransactionHeaderData
  , txInfo :: TransactionInfo
  }
  deriving (Show)

data TransactionHeaderData = TransactionHeaderData
  { hDate :: Day
  , hTitle :: Text
  }
  deriving (Show)

data TransactionInfo = TransactionInfo
  { infoCategory :: Category
  , infoAmount :: Double
  , infoNote :: Text
  }
  deriving (Show)

-- File format example
{--
YYYY-MM-DD Title
    ; Optional description
    Category: CoolMeal
    Amount: 12.34
--}

testParseData :: String
testParseData =
  "2025-05-23 Trader Joe's\n\
  \   Category: CoolMeal\n\
  \   Amount: 12.34\n\
  \   ; Optional description\n\
  \ \n"

testTransactionParser :: Either ParseError Transaction
testTransactionParser = parse parseTransaction "test data" testParseData

-----------------------
-- IO
-----------------------
parseTransactionsFromFile :: FilePath -> IO (Either ParseError [Transaction])
parseTransactionsFromFile fp = readFile fp >>= \c -> return (parse parseManyTransactions "" c)

-- return wraps the return value in the IO monad
-- could also be written as this:
-- parseTransactionsFromFile fp = do
--     content <- readFile fp
--     let parseResult = parse parseManyTransactions "" content
--     return parseResult

-----------------------
-- Reporting
-----------------------
spendingByCategory :: Category -> [Transaction] -> Map Category Double
spendingByCategory cat = foldl (comb cat) M.empty

comb :: Category -> Map Category Double -> Transaction -> Map Category Double
comb c m trans = insertWith (+) c amt m where
    amt = txAmount trans

-----------------------
-- Filtering
-----------------------
filterByMonth :: Integer -> Integer -> [Transaction] -> [Transaction]
filterByMonth year month = filter (isInMonth year month)

isInMonth :: Integer -> Integer -> Transaction -> Bool
isInMonth tYear tMonth transaction = (tYear == y) && (tMonth == fromIntegral m)
  where
    (y, m, _) = toGregorian (txDate transaction)

-----------------------
-- Parsers
-----------------------

parseManyTransactions :: Parser [Transaction]
parseManyTransactions = many (parseTransaction <* optional newline)

parseTransaction :: Parser Transaction
parseTransaction = do
  headerBlock <- parseTransactionHeader
  infoBlock <- parseTransactionInfo

  return
    ( Transaction
        { txDate = hDate headerBlock
        , txTitle = hTitle headerBlock
        , txCategory = infoCategory infoBlock
        , txDescription = infoNote infoBlock
        , txAmount = infoAmount infoBlock
        }
    )

parseTransactionInfo :: Parser TransactionInfo
parseTransactionInfo = TransactionInfo <$> parseCategory <*> parseAmountLine <*> parseNoteContent

parseTransactionHeader :: Parser TransactionHeaderData
parseTransactionHeader = TransactionHeaderData <$> parseDate <*> (space *> parseTitle)

parseNoteContent :: Parser Text
parseNoteContent =
  pack
    <$> ( skipMany1 space
            *> char ';'
            *> space
            *> manyTill anyChar endOfLine
        )

parseCategory :: Parser Category
parseCategory =
  CategoryName . pack
    <$> ( skipMany1 space
            *> string "Category:"
            *> space
            *> manyTill anyChar endOfLine
        )

parseAmountLine :: Parser Double
parseAmountLine = do
  _ <- skipMany1 space
  _ <- string "Amount:"
  _ <- space
  amountString <- parseAmountValue
  _ <- endOfLine
  case readSigned readFloat amountString of
    [(val, "")] -> return val
    _ -> fail "Invalid amount format"

parseAmountValue :: Parser String
parseAmountValue = combine <$> many1 digit <*> char '.' <*> count 2 digit
  where
    combine d _ c = d ++ "." ++ c

parseTitle :: Parser Text
parseTitle = pack <$> manyTill anyChar endOfLine

parseOptionalDescription :: Parser Text
parseOptionalDescription = pack <$> (char ';' *> manyTill anyChar endOfLine)

-- Parse a date string 'YYYY-MM-DD' into a Day type
parseDate :: Parser Day
parseDate = do
  dateStr <-
    parseDateString
      <$> parseYearPart
      <*> (parseDash *> parseMonthOrDayPart)
      <*> (parseDash *> parseMonthOrDayPart)
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr

parseYearPart :: Parser String
parseYearPart = count 4 digit

parseDash :: Parser Char
parseDash = char '-'

parseMonthOrDayPart :: Parser String
parseMonthOrDayPart = count 2 digit

parseDateString :: String -> String -> String -> String
parseDateString y m d = intercalate "-" [y, m, d]

parseAndPrint :: IO ()
parseAndPrint = do
  let filePath = "transactions.fin"
  result <- parseTransactionsFromFile filePath

  case result of
    Left err -> do
      putStrLn $ "Error processing file, this is the error: \n" ++ show err
    Right transactions -> do
      putStrLn "Successfully read file"
      let filteredTransactions = filterByMonth 2025 5 transactions
      let spendCats = spendingByCategory (CategoryName (pack "CoolMeal")) transactions 
      print filteredTransactions
      print spendCats