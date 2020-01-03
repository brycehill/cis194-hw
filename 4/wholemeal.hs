module Wholemeal where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


-- Hint: For this problem you may wish to use the functions iterate
-- and takeWhile. Look them up in the Prelude documentation to see
-- what they do
-- 4 -> 6
-- 6 -> 46
-- 5 -> 30
-- iterate ((+1) . (3*)) .
fun2' :: Integer -> Integer
fun2' n= sum . filter even . takeWhile (>1) . (iterate ((+1) . (3*)) . iterate (`div`2) n)
