{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Finance.Input (readTransactionFile) where

import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Decimal
import Data.Time
import qualified Data.Vector as V
import Finance.Types
import Finance.Utils

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