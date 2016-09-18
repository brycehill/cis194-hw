module Golf where

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
