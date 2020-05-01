{-
 - Excercises concerning lists and operations on lists
 - @author A.E.Veltstra
 - @since 2020-05-01T13:29:00.000EST
 - @version 2020-05-01T13:30:00.000EST
 -}

import Prelude
import Data.Char

digitsOnly :: String -> String
digitsOnly that = [ it | it <- that, isDigit it ]

allCharsAreDigits :: String -> Bool
allCharsAreDigits xs = (xs == [x | x <- xs, isDigit x])

doubleEach :: [Integer] -> [Integer]
doubleEach xs = [ 2*x | x <- xs ]

makeAllCaps :: String -> String
makeAllCaps that = [ toUpper it | it <- that ]

lettersOnly :: String -> String
lettersOnly that = [ it | it <- that, isLetter it ]

makeAllCapsLettersOnly :: String -> String
makeAllCapsLettersOnly that = makeAllCaps (lettersOnly that)

findDivisors :: Integer -> [Integer]
findDivisors that = [ it | it <- [1..that], mod that it == 0 ]

isPrime :: Integer -> Bool
isPrime that 
  | 0 == that = True
  | 1 == that = True
  | otherwise = ([1,that] == findDivisors that)

findPrimes :: (Integer, Integer) -> [Integer]
findPrimes (a, b) = [ x | x <- [a..b], (isPrime x) ]

