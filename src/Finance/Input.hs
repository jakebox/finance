{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Finance.Input (readTransactionFile, parseMonth) where

import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Decimal
import Data.Time (Day)
import Data.Time.Calendar (MonthOfYear)
import qualified Data.Vector as V
import Finance.Types
import Finance.Utils

import Text.Parsec
import qualified Text.Parsec.String as P (Parser)

instance FromField Day where
  parseField :: Field -> Parser Day
  parseField s = do
    d <- parseField s
    case dayFromS d of
      Just date -> pure date
      Nothing -> error "Bad parse date"

instance FromField Category where
  parseField :: Field -> Parser Category
  parseField s = Category <$> parseField s

instance FromField Decimal where
  parseField :: Field -> Parser Decimal
  parseField d = do
    doubleVal :: Float <- parseField d
    pure $ realFracToDecimal 2 doubleVal

instance FromNamedRecord Transaction where
  parseNamedRecord :: NamedRecord -> Parser Transaction
  parseNamedRecord m =
    Transaction
      <$> m .: "Title"
      <*> m .: "Date"
      <*> m .: "Amount"
      <*> m .: "Category"
      <*> m .: "Note"

readTransactionFile :: FilePath -> IO [Transaction]
readTransactionFile fp = do
  csvData <- BS.readFile fp
  case decodeByName csvData of
    Left err -> error err
    Right (_, txs :: V.Vector Transaction) -> pure $ V.toList txs


-- --- Parsers
parseMonth :: P.Parser MonthOfYear
parseMonth = do
  monthStr <- many1 anyChar
  pure $ arbitraryMonToMonth monthStr

arbitraryMonToMonth :: String -> MonthOfYear
arbitraryMonToMonth s 
  | s == "jan" || s == "january" || s == "01" || s == "1" = 1
  | s == "feb" || s == "february" || s == "02" || s == "2" = 2
  | s == "mar" || s == "march" || s == "03" || s == "3" = 3
  | s == "apr" || s == "april" || s == "04" || s == "4" = 4
  | s == "may" || s == "05" || s == "5" = 5
  | s == "jun" || s == "june" || s == "06" || s == "6" = 6
  | s == "jul" || s == "july" || s == "07" || s == "7" = 7
  | s == "aug" || s == "august" || s == "08" || s == "8" = 8
  | s == "sep" || s == "september" || s == "09" || s == "9" = 9
  | s == "oct" || s == "october" || s == "10" || s == "10" = 10
  | s == "nov" || s == "november" || s == "11" || s == "11" = 11
  | s == "dec" || s == "december" || s == "12" || s == "12" = 12
  | otherwise = error $ "Invalid month string: " ++ s
