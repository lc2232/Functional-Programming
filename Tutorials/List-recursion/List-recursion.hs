

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

member' :: [String] -> String -> Bool
member'    []  _ = False 
member' (x:xs) y = y == x || member' (xs) y

remove :: [String] -> String -> [String]
remove [] _     = []
remove (x:xs) y
    | x == y    = xs
    | otherwise = x:remove (xs) y

------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True
members xs (y:ys)
    | x == y    = members (tail xs) ys --pops the items off the list kinda
    | otherwise = member xs y && members xs ys
    where 
     x = head xs

members' :: [String] -> [String] -> Bool
members' xs    []  = True -- base case 
members' xs (y:ys) = member xs y && members xs ys

-- [Joe, Howard, Mark] [Joe, Mark]
--member [Joe, Howard, Mark] Joe && members [Joe, Howard, Mark] [Mark]
-- member [Joe, Howard, Mark] Mark && members [Joe, Howard, Mark] []
-- member [Joe, Howard, Mark] [] && members [Joe, Howard, Mark] [] 


removeAll :: [String] -> [String] -> [String]
removeAll xs [] = xs
removeAll xs (y:ys) = remove (removeAll xs ys) y

-- [Joe, Howard, Mark] [Joe, Mark]
-- remove (removeAll [Joe, Howard, Mark] [Mark]) Joe
-- remove (removeAll [Joe, Howard, Mark] []) Mark --> remove [Joe, Howard, Mark] Mark -->
-- [Joe, Howard, Mark]


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = True
before [] _ = True
before (x:xs) (y:ys)
    | x < y = True
    | x == y = before xs ys --same letter hence we need to check the next letters
    | otherwise = False

before' :: [Char] -> [Char] -> Bool
before' _ [] = True
before' [] _ = True
before' (x:xs) (y:ys) = (x < y || False) || before' xs ys

sorted :: [String] -> Bool
sorted [] = error "Empty list"
sorted (x:xs)
    | xs == []  = True --one item (base case)
    | otherwise = before x (head xs) && sorted xs-- two or more

-- ["Charles","Fitzwilliam","George","William"]
-- before Charles Fitzwilliam && sorted [Fitzwilliam,George,William]
-- before Fitzwilliam George && sorted [George,William]
-- before George William && sorted [William]
-- before George William && sorted []

