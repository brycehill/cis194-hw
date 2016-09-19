module Golf where

import Data.List
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = [everyNth n xs | n <- [1..length xs]]


everyNth :: Int -> [a] -> [a]
everyNth n xs = [xs !! idx | idx <- [n-1, n-1+n..length xs - 1]]


-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs) = (if y > x && y > z then [y] else []) ++ localMaxima (y:z:zs)
localMaxima _ = []


-- histogram [1,1,1,5] ==
--  *
--  *
--  * *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--  *
--  *
--  * *
--  ****** *
-- ==========
-- 0123456789

-- histogram [3,5]
-- " * * \n==========\n0123456789\n

-- putStr (histogram [3,5])


histogram :: [Integer] -> String
histogram ns = intercalate "\n" $
  reverse (map replaceWithStar $ transpose
   . group $ sort ns) ++ ["==========","0123456789\n"]


replaceWithStar :: [Integer] -> String
replaceWithStar = foldl (replace "*") spaces
  where spaces = replicate 10 ' '


replace :: String -> String -> Integer -> String
replace newStr s n = take (fromIntegral n) s ++ newStr ++ drop (fromIntegral (n+1)) s
