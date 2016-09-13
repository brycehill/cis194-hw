module CC where

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [2,1])

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sum xxs)
  where xxs = [toDigits x | x <- xs]

validate :: Integer -> Bool
validate cc = sumDigits (doubleEveryOther (toDigits cc)) `mod` 10 == 0
