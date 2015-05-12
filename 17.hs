import Data.Char

numToWord n =
    case toDigits n of
      [ones]           -> uniqueNums !! ones
      [1, ones]        -> uniqueNums !! (ones + 10)
      [tens, 0]        -> tenMults !! tens
      [tens, ones]     -> (tenMults !! tens) ++ "-" ++ (uniqueNums !! ones)
      [hundreds, 0, 0] ->
          (uniqueNums !! hundreds) ++ " hundred"
      [hundreds, tens, ones] ->
          (uniqueNums !! hundreds) ++ " hundred and " ++ (numToWord $ tens * 10 + ones)
      [1, 0, 0, 0] -> "one thousand"
    where
      toDigits = (map digitToInt) . show
      uniqueNums =
          ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
           "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
           "sixteen", "seventeen", "eighteen", "nineteen"]
      tenMults =
          ["zero", "ten", "twenty", "thirty", "forty", "fifty",
           "sixty", "seventy", "eighty", "ninety"]


main = print . length . filter (\c -> c >= 'a' && c <= 'z') . concat . map numToWord $ [1..1000]
