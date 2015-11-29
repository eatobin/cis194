-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, l) = (==) (toRevDigits n) l

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3,2,1]), (1234, [4,3,2,1]), (5, [5]), (0, []), ((-17), [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (l1, l2) = (==) (doubleEveryOther l1) l2

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([1,2,3], [1,4,3]), ([1,2,3,4], [1,4,3,8]), ([1,2,3,4,5,6,7], [1,4,3,8,5,12,7]), ([5], [5]), ([], [])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (l, n) = sumDigits l == n

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([1,2,3], 6), ([10,2,33,4], 13), ([5], 5), ([], 0)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: Integer -> Bool
testLuhn n = luhn n

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [4662110665499438, 645937, 1859]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, p1, p2, p3, m) = hanoi n p1 p2 p3 == m

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(1, "a", "b", "c", [("a","c")]), (2, "a", "b", "c", [("a","b"),("a","c"),("b","c")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
