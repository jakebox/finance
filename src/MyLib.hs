{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MyLib (someFunc) where

import Data.List
import Data.Text (Text, pack)
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Numeric (readFloat, readSigned)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Internal types
data Transaction = Transaction
  { txDate :: Day
  , txTitle :: Text
  , txDescription :: Text
  , txCategory :: Text
  , txAmount :: Double
  }
  deriving (Show)

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
  { infoCategory :: Text
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
-- Parsers
-----------------------

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

parseCategory :: Parser Text
parseCategory =
  pack
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
parseAmountValue = comb <$> many1 digit <*> char '.' <*> count 2 digit
  where
    comb d _ c = d ++ "." ++ c

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
