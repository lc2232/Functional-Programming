

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1

-- implementation of ditch
ditch :: Int -> [a] -> [a]
ditch _ [] = []
ditch n (x:xs)
    | n == 1 = xs
    | otherwise = ditch (n-1) xs

-- implementation of the (!!)
at :: [a] -> Int -> a
at xs i
  | i < 0 || i>length(xs) = error "Cringe" --guard against bad i inputs
  | i == 0 = (head xs)
  | otherwise = at (tail xs) (i-1)


------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find n [] = error "No related item"
find n (x:xs)
    | n == (fst x) = (snd x)
    | otherwise = find n xs
    
which :: Eq a => a -> [a] -> Int
which _ [] = error "Not in list"
which item (x:xs)
    | item == (x) = 0
    | otherwise = 1 + which item xs


member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member (xs) y

remove :: Eq a => [a] -> a -> [a]
remove [] _     = []
remove (x:xs) y
    | x == y    = xs
    | otherwise = x:remove (xs) y
    
before :: Ord a => [a] -> [a] -> Bool
before _ [] = True
before [] _ = True
before (x:xs) (y:ys)
    | x < y = True
    | x == y = before xs ys --same letter hence we need to check the next letters
    | otherwise = False
    
--sorted :: Ord a => [[a]] -> Bool
sorted [] = error "Empty list"
sorted (x:xs)
    | xs == []  = True --one item (base case)
    | otherwise = before x (head xs) && sorted xs-- two or more

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = undefined
merge [] ys = undefined
merge (x:xs) (y:ys) = undefined

minus :: Ord a => [a] -> [a] -> [a]
minus = undefined

msort :: Ord a => [a] -> [a]
msort = undefined
