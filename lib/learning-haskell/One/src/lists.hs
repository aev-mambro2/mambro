{-
 - Excercises concerning lists and operations on lists
 - @author A.E.Veltstra
 - @since 2020-05-01T13:29:00.000EDT
 - @version 2020-05-21T15:02:00.000EDT
 -}

import Prelude
import Data.Char

digitsOnly :: String -> String
digitsOnly x = [ it | it <- x, isDigit it ]

allCharsAreDigits :: String -> Bool
allCharsAreDigits xs = (xs == [x | x <- xs, isDigit x])

doubleEach :: [Integer] -> [Integer]
doubleEach xs = [ 2*x | x <- xs ]

makeAllCaps :: String -> String
makeAllCaps x = [ toUpper y | y <- x ]

lettersOnly :: String -> String
lettersOnly x = [ y | y <- x, isLetter y ]

makeAllCapsLettersOnly :: String -> String
makeAllCapsLettersOnly x = makeAllCaps (lettersOnly x)

-- |Find all numbers that divide the input without a remainder.
findDivisors :: Integer -> [Integer]
findDivisors x = [ y | y <- [1..x], mod x y == 0 ]

-- |Whether or not a number is a prime (i.e.: can be divided
-- only by 1 and itself).
isPrime :: Integer -> Bool
{-
--This implementation uses guards
isPrime x 
  | 0 == x = True
  | 1 == x = True
  | otherwise = ([1,x] == findDivisors x)
-}
--This implementation uses pattern matching
isPrime 0 = True
isPrime 1 = True
isPrime x = ([1,x] == findDivisors x)

-- |Tests isPrime and reports the test result
testIsPrime :: IO ()
testIsPrime = do
  let a = isPrime 9
  let b = isPrime 99
  let c = isPrime 97
  let test = a == False && b == False && c == True
  let result = if test == True 
               then "Succeeded" 
               else "Failed"
  putStrLn (result ++ " Test: testIsPrime")


findPrimes :: (Integer, Integer) -> [Integer]
findPrimes (x, y) = [ z | z <- [x..y], (isPrime z) ]

test :: IO ()
test = do
  testIsPrime

