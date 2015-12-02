{-# OPTIONS_GHC -Wall #-}
module LS02 where

strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs
                   in  len_rest + 1

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

rectangle :: Double -> Double -> Double -> Double
rectangle h w d =
  let sideArea = 2 * d * h
      frontArea = 2 * w * h
      topArea = 2 * d * w
  in  sideArea + frontArea + topArea

frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

intListSum :: [Int] -> Int
intListSum [] = 0
intListSum (x:xs) = x + intListSum xs

sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums   -- the acc. starts at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc   -- empty list: return the accumulated sum
        go acc (x:xs)
         | acc >= 20 = acc
         | otherwise = go (acc + x) xs

-- sumTo20 [4,9,10,2,8] == 23
-- sumTo20 [4,59,10,2,8] == 63

strange :: a -> b
strange = error "impossible!"

limited :: a -> a
limited x = x

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

mapIntList :: (a -> b) -> [a] -> [b]
mapIntList _ []     = []
mapIntList f (x:xs) = f x : mapIntList f xs

--exampleList = [(-1), 2, 6]
--mapIntList (+1) exampleList
--mapIntList abs  exampleList
--mapIntList (^2) exampleList

-- My example
eatMap :: [Integer]
eatMap = map (\x -> x*10) (map (\x -> x+1) [1,2,3])

-- or
eatMap' :: [Integer] -> [Integer]
eatMap' xs = map mee xs
  where mee x = x + 1

-- or
eatMap'' :: [Integer] -> [Integer]
eatMap'' = map (+1)

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

-- foldl :: (o -> i -> o) -> o -> [i] -> o
-- foldr :: (i -> o -> o) -> o -> [i] -> o

myFoldR :: [Integer]
myFoldR = foldr (\x acc -> acc ++ [x]) [60] [66,99,44]
-- [60,44,99,66]

myFoldL :: [Integer]
myFoldL = foldl (\acc x -> x : acc) [60] [66,99,44]
-- [44,99,66,60]

myFoldL' :: [Integer]
myFoldL' = foldl (\acc x -> acc ++ [x]) [60] [66,99,44]
-- [60,66,99,44]


keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x:xs)
  | x > 0     = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x:xs)
  | even x    = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs

filterIntList :: (a -> Bool) -> [a] -> [a]
filterIntList _ [] = []
filterIntList p (x:xs)
  | p x       = x : filterIntList p xs
  | otherwise = filterIntList p xs
-- filterIntList (\x -> x > 2) [1,2,3,4] == [3,4]

myFold :: (a -> b -> b) -> b  -> [a] -> b
myFold _ z []     = z
myFold f z (x:xs) = f x (myFold f z xs)
-- myFold (+) 0 [1,2,3] == 6
-- myFold (*) 1 [1,22,3] == 66

-- OR

myFold2 :: (a -> a -> a) -> a  -> [a] -> a
myFold2 _ z []     = z
myFold2 f z (x:xs) = f x (myFold2 f z xs)
-- myFold2 (+) 100 [1,2,3,4] == 110
