import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

main = print $ length $ filter (\(y, m, d) -> d == 7) $ dates
    where dates = [ toWeekDate $ fromGregorian y m 1 | y <- [1901..2000], m <- [1..12] ]
