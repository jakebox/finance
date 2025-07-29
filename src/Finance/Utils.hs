module Finance.Utils (dayFromS, today) where

import Data.Time

dayFromS :: String -> Maybe Day
dayFromS = parseTimeM True defaultTimeLocale "%F"

today :: IO Day
today = getCurrentTimeZone >>= \t -> localDay . utcToLocalTime t <$> getCurrentTime
