{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MyLib
  ( parseAdd
  , AddCommandOptions (..)
  , ReportCommandOptions (..)
  , Command (..)
  , parseReport
  ) where

import Data.List
import Data.Map.Strict (Map, insertWith)
import qualified Data.Map.Strict as M
import Data.Text (Text, null, pack)
import qualified Data.Text.IO as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric (readFloat, readSigned, showFFloat)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Internal types
data Transaction = Transaction
  { txDate :: Day
  , txTitle :: Text
  , txDescription :: Text
  , txCategory :: Category
  , txAmount :: Double
  }
  deriving (Show)

categoryToText :: Category -> Text
categoryToText (CategoryName name) = name

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
-- Formatting output
-----------------------
formatTransaction :: Transaction -> Text
formatTransaction t =
  (formatTransactionDate (txDate t) <> pack " " <> txTitle t)
    <> keyValueNewline (pack "Category") (categoryToText (txCategory t))
    <> keyValueNewline (pack "Amount") (formatTransactionAmount (txAmount t))
    <> (indentationOutputNewline <> pack "; " <> txDescription t)
    <> pack "\n"
  where
    keyValueNewline key value =
      indentationOutputNewline <> key <> pack ": " <> value

formatTransactionForCLI :: Transaction -> Text
formatTransactionForCLI t =
  formatTransactionDate (txDate t)
    <> pack " "
    <> txTitle t
    <> pack " - $"
    <> formatTransactionAmount (txAmount t)
    <> pack "\n   Category: "
    <> categoryToText (txCategory t)
    <> ( if Data.Text.null (txDescription t)
          then pack ""
          else pack " (" <> txDescription t <> pack ")"
       )

formatTransactionDate :: Day -> Text
formatTransactionDate d = pack (formatTime defaultTimeLocale "%Y-%m-%d" d)

formatTransactionAmount :: Double -> Text
formatTransactionAmount a = pack (showFFloat (Just 2) a "")

indentationOutputNewline :: Text
indentationOutputNewline = pack "\n" <> pack (replicate 4 ' ')

-----------------------
-- Reporting
-----------------------
spendingByCategory :: [Transaction] -> Map Category Double
spendingByCategory = foldl addToMap M.empty
  where
    addToMap cur_map trans = insertWith (+) (txCategory trans) (txAmount trans) cur_map

-----------------------
-- Filtering
-----------------------
filterByMonth :: Integer -> Integer -> [Transaction] -> [Transaction]
filterByMonth year month = filter (isInMonth year month)

isInMonth :: Integer -> Integer -> Transaction -> Bool
isInMonth tYear tMonth transaction = (tYear == y) && (tMonth == fromIntegral m)
  where
    (y, m, _) = toGregorian (txDate transaction)

filterByCategory :: Category -> [Transaction] -> [Transaction]
filterByCategory category = filter (\t -> txCategory t == category)

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

data Command
  = AddCommand AddCommandOptions
  | ReportCommand ReportCommandOptions
  deriving (Show)

data ReportCommandOptions = ReportCommandOptions
  { reportType :: String
  , reportDateBlock :: Maybe String
  , reportCategory :: Maybe String
  }
  deriving (Show)

data AddCommandOptions = AddCommandOptions
  { addDate :: String
  , addTitle :: String
  , addNote :: Maybe String
  , addCategory :: String
  , addAmount :: String
  }
  deriving (Show)

safeParseDate :: String -> Maybe Day
safeParseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

safeParseCategory :: String -> Maybe Category
safeParseCategory catStr =
  Just (CategoryName (pack catStr)) -- Assuming the category is always valid

safeParseAmount :: String -> Maybe Double
safeParseAmount amountStr =
  case readSigned readFloat amountStr of
    [(val, "")] -> Just val
    _ -> Nothing -- Invalid amount format, return Nothing

convertOptionsToTransaction :: AddCommandOptions -> Maybe Transaction
convertOptionsToTransaction opts = do
  parsedDate <- safeParseDate (addDate opts)
  parsedCategory <- safeParseCategory (addCategory opts)
  parsedAmount <- safeParseAmount (addAmount opts)
  let packedTitle = pack (addTitle opts)
  let packedDescription = maybe (pack "") pack (addNote opts)

  return
    Transaction
      { txDate = parsedDate
      , txTitle = packedTitle
      , txDescription = packedDescription
      , txCategory = parsedCategory
      , txAmount = parsedAmount
      }

applyDateBlockFilter :: Maybe String -> [Transaction] -> [Transaction]
applyDateBlockFilter block transactions = case block of
  Just s -> case s of
    "month" -> filterByMonth 2024 4 transactions
    _ -> error "Not a valid time block"
  Nothing -> transactions

applyCategoryFilter :: Maybe String -> [Transaction] -> [Transaction]
applyCategoryFilter category transactions = case category of
  Just c -> filterByCategory (CategoryName (pack c)) transactions
  _ -> transactions

printTransactions :: [Transaction] -> IO ()
printTransactions = mapM_ (TIO.putStrLn . formatTransactionForCLI)

printCategories :: Map Category Double -> IO ()
printCategories categoriesMap =
  mapM_
    (\(cat, amt) -> TIO.putStrLn (categoryToText cat <> pack ": $" <>formatTransactionAmount amt))
    (M.toList categoriesMap)

parseReport :: ReportCommandOptions -> IO ()
parseReport opts = do
  let filePath = "transactions.fin"
  result <- parseTransactionsFromFile filePath

  case result of
    Left err -> do
      putStrLn $ "Error processing file: \n" ++ show err
    Right transactions -> do
      let x = applyDateBlockFilter (reportDateBlock opts) transactions
      let y = applyCategoryFilter (reportCategory opts) x

      case reportType opts of
        "list" -> do
          printTransactions y
        "category" -> do
          printCategories (spendingByCategory y)
        _ -> putStrLn "Invalid report type."

parseAdd :: AddCommandOptions -> IO ()
parseAdd opts = do
  let trans = convertOptionsToTransaction opts
  case trans of
    Just transaction -> do
      T.appendFile "./transactions.fin" (formatTransaction transaction <> pack "\n")
      putStrLn "Successfully wrote transaction"
    Nothing ->
      putStrLn
        "Failed to parse Add options into Transaction. Please check the input values."