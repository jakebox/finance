module Finance.Utils (dayFromS) where

import Data.Time

dayFromS :: String -> Maybe Day
dayFromS = parseTimeM True defaultTimeLocale "%F"