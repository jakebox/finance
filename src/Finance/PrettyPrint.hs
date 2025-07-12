module Finance.PrettyPrint (ppTransaction, ppAggregatedSpending) where

import qualified Data.Text as T
import Data.Decimal (Decimal)
import Data.Map (Map)
import qualified Data.Map as M

import Finance.Types

{-

$24.55 - Trader Joe's | 2025-05-26 (Groceries)

-}
ppTransaction :: Transaction -> T.Text
ppTransaction tx = (T.pack $ show (tx.txAmount)) 
                        <> T.pack "-" 
                        <> txTitle tx
                        <> T.pack " | "
                        <> (T.pack $ show (tx.txDate))
                        <> (T.pack $ show (tx.txCategory))

ppAggregatedSpending :: AggregatedSpending -> T.Text
ppAggregatedSpending ag 
    | M.null ag = T.empty
    | otherwise = T.intercalate "\n" (header : lines)
            where
                lines = M.foldrWithKey acc [] ag
                categoryPad = 16
                header = (T.justifyLeft categoryPad ' ' "Category") <> (T.justifyRight 7 ' ' "Amount")
                acc :: Category -> Decimal -> [T.Text] -> [T.Text]
                acc cat amt acc_lines =
                    let categoryText = getCategoryText cat
                        amountText = T.pack $ show amt
                        newLine = T.justifyLeft categoryPad ' ' (categoryText <> ": ") <> T.justifyRight 7 ' ' amountText
                    in newLine : acc_lines


{-
Discretionary:  $55.43
Food:          $519.56
Necessities:    $34.10
Social:         $37.23
Transport:     $273.65
-}