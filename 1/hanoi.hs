module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start tmp end 
  | n == 0 = []
  | otherwise = hanoi (n - 1) start end tmp ++ [(start, end)] ++ hanoi (n - 1) tmp start end

