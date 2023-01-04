

------------------------- Exercise 1

square :: Int -> Int
square x = x*x


pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = square a + square b == square c

------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

euclid :: Int -> Int -> Int
euclid x y
    | x <= 0 || y <= 0 = error "euclid: non-positive integer in input"
    | x == y = x
    | x <  y = euclid x (y - x)
    | x >  y = euclid (x - y) y

power_ :: Int -> Int -> Int
power_ x y
    | y <  0    = error "power_: negative exponent"
    | y == 0    = 1
    | even y    = square (power_ x (div y 2))
    | otherwise = square (power_ x (div y 2)) * x
    
--note: you will need to create your own cases,
--      replacing the equals (=) sign with guards


------------------------- Exercise 3

range :: Int -> Int -> [Int]
range n m
    | m < n = []
    | m >= n = n : range (n+1) m
    | otherwise = error "M < N"
--note: you will need to create your own guards
--      and add your own parameters

times :: [Int] -> Int
times [] = 1
times (x:xs) = x * times xs
--note: you will need to create your own pattern-matching

fact :: Int -> Int
fact n = times (range 1 n)