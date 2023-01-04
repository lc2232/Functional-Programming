
------------------------- Exercise 1
double :: Int -> Int
double x = x*2


doubles :: [Int] -> [Int]
-- doubles [] = []
-- doubles (x:xs) = double x:doubles xs
doubles = map double


odds :: [Int] -> [Int]
-- odds [] = []
-- odds (x:xs) 
--    | odd x = x : odds xs
--    | otherwise = odds xs
odds = filter odd

doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds (x:xs) = doubles (odds (x:xs))

------------------------- Exercise 2

shorts :: [String] -> [String]
shorts = filter length4
    where length4 x = length x < 6

squarePositives :: [Int] -> [Int]
squarePositives = undefined

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums = undefined

------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove [] _     = []
remove (x:xs) y = filter (/=y) (x:xs)


-- checks each element is in string
-- removes each element 

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] _ = []
removeAll xs [] = xs
removeAll (x:xs) (y:ys) = removeAll (remove (x:xs) y) ys



numbered :: [a] -> [(Int,a)]
numbered xs = zip[1..length xs] xs

everyother :: [a] -> [a]
everyother xs = map snd (filter p (zip [1..length xs] xs))
    where
        p (i,_) = odd i

same :: Eq a => [a] -> [a] -> [Int]
same = undefined

-- sameLetters :: [a] -> [a]
-- sameLetters xs ys = sameChar (zip [1..length xs] xs) (zip [1..length ys] ys)
    -- where
        -- sameChar (_,i) (_,j) = i==j


