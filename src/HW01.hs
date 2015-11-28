{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- OR!

-- Using divMod
lastDigit2 :: Integer -> Integer
lastDigit2 n = snd (divMod n 10)

dropLastDigit2 :: Integer -> Integer
dropLastDigit2 n = fst (divMod n 10)

-- Exercise 2 -----------------------------------------

-- Produce a list of the reversed digits of a number
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- added bonus:

-- Function composition
-- toDigits :: Integer -> [Integer]
-- toDigits n = (reverse . toRevDigits) n

-- Function composition - Pointfree!
toDigits' :: Integer -> [Integer]
toDigits' = reverse . toRevDigits

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:zs) = x : (y * 2) : doubleEveryOther zs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
-- sumDigits [] = 0
-- sumDigits (x:xs) = dropLastDigit x + lastDigit x + sumDigits xs
sumDigits = foldr (\ x -> (+) (dropLastDigit x + lastDigit x)) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = mod (sumDigits (doubleEveryOther (toRevDigits n))) 10 == 0
-- 4662110665499438 True
-- 645937 True
-- 1859 True

-- Exercise 6 -----------------------------------------

---- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

--hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
--hanoi 1 p1 _ p3 = [(p1,p3)]
--hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ (hanoi 1 p1 p2 p3) ++ (hanoi (n-1) p2 p1 p3)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b a c
