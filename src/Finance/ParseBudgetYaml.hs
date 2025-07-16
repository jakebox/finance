{-# OPTIONS_GHC -Wno-orphans #-}

module Finance.ParseBudgetYaml (parseBudgetYaml) where

import Data.Aeson.Types
import Data.Decimal
import Data.Map.Strict (Map)
import Data.Scientific
import Data.Yaml
import Finance.Types

instance FromJSON Category

instance FromJSONKey Category where
  fromJSONKey = FromJSONKeyTextParser (pure . Category)

instance FromJSON Decimal where
  parseJSON value = case value of
    Number s -> pure $ realFracToDecimal 2 (toRealFloat s)
    _ -> error "bad parse decimal"

parseBudgetYaml :: FilePath -> IO (Either ParseException BudgetMap)
parseBudgetYaml = decodeFileEither
