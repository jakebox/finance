{-# OPTIONS_GHC -Wno-orphans #-}

module Finance.ParseBudgetYaml (ParsedBudgets(..)) where

import GHC.Generics
import Finance.Types
import Finance.Input
import Data.Map.Strict (Map)
import Data.Decimal
import Data.Time
import Data.Time.Calendar.Month
import qualified Data.Text as T

import Data.Yaml
import Data.Aeson.Types
import Data.Scientific
import Finance.Input (stringToYearMonth)

newtype YYYYMM = YYYYMM T.Text
  deriving (Eq, Show, Generic, Ord)

type ParsedBudgets = Map YYYYMM (Map Category Decimal)

instance FromJSON Category
instance FromJSON Budget
instance FromJSON YYYYMM

instance FromJSONKey YYYYMM where
  fromJSONKey = FromJSONKeyTextParser parseYYYYMMText

instance FromJSONKey Category where
  fromJSONKey = FromJSONKeyTextParser (pure . Category)

parseYYYYMMText :: T.Text -> Parser YYYYMM
parseYYYYMMText txt = pure (YYYYMM txt)

instance FromJSON Decimal where
  parseJSON value = case value of
    Number s -> pure $ realFracToDecimal 2 (toRealFloat s)
    _ -> error "bad parse decimal"
