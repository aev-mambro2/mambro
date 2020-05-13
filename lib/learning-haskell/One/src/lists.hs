{-
 - Excercises concerning lists and operations on lists
 - @author A.E.Veltstra
 - @since 2020-05-01T13:29:00.000EDT
 - @version 2020-05-12T14:36:00.000EDT
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

findDivisors :: Integer -> [Integer]
findDivisors x = [ y | y <- [1..x], mod x y == 0 ]

isPrime :: Integer -> Bool
isPrime x 
  | 0 == x = True
  | 1 == x = True
  | otherwise = ([1,x] == findDivisors x)

findPrimes :: (Integer, Integer) -> [Integer]
findPrimes (x, y) = [ z | z <- [x..y], (isPrime z) ]

