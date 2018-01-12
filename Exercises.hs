module Exercises where

isPalindrom :: (Ord a) => [a] -> Bool
isPalindrom xs = xs == reverse xs

say :: Int -> String
say 0 = "nula"
say 1 = "jedna"
say 2 = "dva"
say _ = "vela"


fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

dajMeno :: Char -> String
dajMeno 'A' = "Adam"
dajMeno 'B' = "Boris"
dajMeno 'C' = "Cyril"
dajMeno _ = "Neznam"

sumPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumPairs a b = (fst a + fst b, snd a + snd b)

sumPairs4 :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumPairs4 (a, b) (c, d) = (a + c, b + d)

prvy :: (Int, Int, Int) -> Int
prvy (a, _, _) = a

druhy :: (Int, Int, Int) -> Int
druhy (_, b, _) = b

treti :: (Int, Int, Int) -> Int
treti (_, _, c) = c

mult3 :: Int -> Int -> Int -> Int
mult3 a b c = 100 * a + 10 * b + c

mult2 :: Int -> Int -> Int
mult2 a b = a * b

test :: [t] -> String
test [] = "prazdny"
test (_:[]) = "jeden prvok"
test (_:_:[]) = "dva prvky"
test _ = "plny"

plus1 :: Num t => [t] -> [t]
plus1 [] = []
plus1 (x:xs) = (x + 1) : plus1 xs

length' :: Num a => [t] -> a
length' [] = 0
length' (_:xs) = 1 + length' xs

maximum' :: Ord t => [t] -> t
maximum' [] = error "chyba"
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)

reverse' :: [t] -> [t]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [t] -> [t1] -> [(t, t1)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) =  (x,y) : zip' xs ys

elem' :: Int -> [Int] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e == x then True else elem' e xs

sum' :: [Int] -> Int
sum' xs = sumAcc' 0 xs

sumAcc' :: Int -> [Int] -> Int
sumAcc' n [] = n
sumAcc' n (x:xs) = sumAcc' (n + x) xs

filter' :: Int -> [Int] -> [Int]
filter' _ [] = []
filter' n (x:xs) = if n == x then filter' n xs else x : filter' n xs
