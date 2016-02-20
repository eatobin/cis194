module LS01 where

ex00 :: Int
ex00 = (*) 4 3
-- 12
ex01 :: Double
ex01 = (/) 20 5
-- 4.0
ex02 = div 20 5
-- 4
ex27 = mod 20 5
-- 0
ex28 = div 135 10
-- 13
ex29 = mod 135 10
-- 5
ex03 :: Bool
ex03 = (==) 5 6
-- False
ex04 = (==) 5 5
-- True
ex05 = (==) 3 (-3)
-- False
ex06 = (==) (-3) (-3)
-- True
ex07 = (-) 16 4
-- 12
ex08 = (+) 5 6
-- 11
ex09 = (+) ((+) 5 6) 3
--14
ex10 = mod 20 5
-- 0
ex11 = mod 20 9
-- 2
ex12 = (/=) 4 8
-- True
ex13 = (+) ex07 ex08
-- 23
-- ex14 :: Int
-- ex14 = (+) ex14 1
-- ex15 :: Bool
ex15 = (^) 7 3
-- 343
ex16 = (>) 6 4
-- True
ex17 :: Int -> Bool
ex17 n = (>) n 4
-- ex17 3 -> False
-- ex17 4 -> False
-- ex17 5 -> True
ex18 :: (Int, Int) -> Int
ex18 (x, y) = (+) x y
-- ex18 (7, 8) -> 15
ex19 :: (Int, Char)
ex19 = (3, 'x')
-- (3,'x')
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = (+) n (sumtorial ((-) n 1))
ex20 = (++) [3,4,5] [60,70]
-- [3,4,5,60,70]
ex21 = (:) 6 [7,8,9]
-- [6,7,8,9]
hailstone :: Integer -> Integer
hailstone n
  | (==) (mod n 2) 0 = div n 2
  | otherwise          = (+) ((*) 3 n) 1
-- hailstone 19 -> 58
foo :: Integer -> Integer
foo 0 = 16
foo 1
  | (<) "Haskell" "C++" = 3
  | otherwise           = 4
foo n
  | (<) n 0        = 0
  | mod n 17 == 2  = -43
  | otherwise      = (+) n 3
f :: Int -> Int -> Int
f x y = x + y
ex22 = f 3 4
-- ex22 -> 7
ex23 = f (4 + 5) 1
-- ex23 -> 10
f1 :: Int -> Int -> Int -> Int
f1 x y z = (x + y) + z
ex24 = f1 1 2 3
ex25 = 2 : 3 : 4 :[]
ex26 = 3 : (1 : [])
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
-- hailstoneSeq 19 -> [19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = (+) (intListLength xs) 1
-- intListLength (hailstoneSeq 19) -> 21
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs
-- sumEveryTwo (hailstoneSeq 19) -> [77,117,66,45,69,39,60,15,24,6,1]
-- The number of hailstone steps needed to reach 1 from a starting
-- number.
hailstoneLen :: Integer -> Integer
hailstoneLen n = (-) (intListLength (hailstoneSeq n)) 1
-- hailstoneLen 19 -> 20
revList :: [Int] -> [Int]
revList [] = []
revList (x:xs) = x : revList xs
-- revList [] -> []
-- revList [1,2,3,4,5] -> [1,2,3,4,5]
revList2 :: [Int] -> [Int]
revList2 xs = reverse xs
-- revList2 [] -> []
-- revList2 [1,2,3,4,5] -> [5,4,3,2,1]
