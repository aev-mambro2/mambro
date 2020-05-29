module Lists (
    digitsOnly,
    allCharsAreDigits,
    makeAllCaps,
    lettersOnly,
    makeAllCapsLettersOnly,
    isPrime,
    findPrimes,
    test
  ) where
{-
 - Excercises concerning lists and operations on lists
 - @author A.E.Veltstra
 - @since 2020-05-01T13:29:00.000EDT
 - @version 2020-05-21T15:02:00.000EDT
 -}

import Prelude
import Data.Char
import qualified Control.Exception as Ex

-- |Evaluates the passed-in contents and returns only those
-- characters that are digits.
digitsOnly :: String -> String
digitsOnly x = [ it | it <- x, isDigit it ]

-- |Tests digitsOnly and reports the outcome.
testDigitsOnly :: IO ()
testDigitsOnly = do
  let positiveSubject = "abc123doremi"
  let positiveOutput = digitsOnly positiveSubject
  let positiveTest = "123" == positiveOutput
  let negativeSubject = "abcdef ghj-()@"
  let negativeOutput = digitsOnly negativeSubject
  let negativeTest = "" == negativeOutput
  let test = positiveTest == True && negativeTest == True
  let result = if test == True
               then "Succeeded"
               else "Failed"
  putStrLn (result ++ " Test: testDigitsOnly")

-- |Evaluates whether all characters in the input are digits.
allCharsAreDigits :: String -> Bool
allCharsAreDigits xs = (xs == [x | x <- xs, isDigit x])

-- |Tests allCharsAreDigits and reports the outcome.
testAllCharsAreDigits :: IO ()
testAllCharsAreDigits = do
  let positiveSubject = "94724791"
  let positiveOutcome = allCharsAreDigits positiveSubject
  let positiveTest = True == positiveOutcome
  let negativeSubject = "af6af9fiafna9f8a7df6"
  let negativeOutcome = allCharsAreDigits negativeSubject
  let negativeTest = False == negativeOutcome
  let test = positiveTest == True && negativeTest == True
  let result = if test == True
               then "Succeeded"
               else "Failed"
  putStrLn (result ++ " Test: testAllCharsAreDigits")

-- |Doubles each of the input numbers
doubleEach :: [Integer] -> [Integer]
doubleEach xs = [ 2*x | x <- xs ]

-- |Tests doubleEach positively and reports the outcome
testDoubleEach :: IO ()
testDoubleEach = do
  let input = 8:9:13:78:[]
  let doubled = doubleEach input
  let expected = 16:18:26:156:[]
  let test = expected == doubled
  let result = if test == True
               then "Succeeded"
               else "Failed"
  putStrLn (result ++ " Test: testDoubleEach")

-- |Tests doubleEach negatively and reports the outcome.
-- This tests causes and catches an error.
{-
testDoubleEachBad :: IO ()
testDoubleEachBad = do
  let badInput = "badInput"
  Ex.catch (show (doubleEach badInput))
           (\err -> do
                      let msg = show err
                      let expected = ""
                      let test = expected == msg
                      let result = if test == True
                                   then "Succeeded"
                                   else "Failed"
                      putStrLn (result ++ " Test: testDoubleEachBad 2")
           )
-}  

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

-- |Return the first number in a list + 1. If none found, return 0.
doExercise71 :: [Integer] -> Integer
doExercise71 [] = 0
doExercise71 (x:_) = x + 1
                       
-- |Tests exercise 7.1.
testExercise71 :: IO ()
testExercise71 = do
  let input1 = 9:3:8:[]
  let output1 = doExercise71 input1
  let test1 = 10 == output1
  let input2 = []
  let output2 = doExercise71 input2
  let test2 = 0 == output2
  let result = if test1 == True && test2 == True
               then "Succeeded" 
               else "Failed"
  putStrLn (result ++ " Test: testExercise71")
  

-- |Add the 1st 2 integers in a list, or return head element, or 0
doExercise72 :: [Integer] -> Integer
doExercise72 [] = 0
doExercise72 [x] = x
{-
doExercise72 them
  | 1 == (length them) = them!!0
  | 1 <= (length them) = them!!0 + them!!1
-}
--doExercise72 (x:xs) = x + (xs!!0)
doExercise72 (x:y:_) = x + y

-- |Tests exercise 7.2.
testExercise72 :: IO ()
testExercise72 = do
  let input1 = [5,3,7]
  let input2 = [4]
  let input3 = []
  let output1 = doExercise72 input1
  let output2 = doExercise72 input2
  let output3 = doExercise72 input3
  let test1 = 8 == output1
  let test2 = 4 == output2
  let test3 = 0 == output3
  let result = if test1 == True && test2 == True && test3 == True
               then "Succeeded" 
               else "Failed"
  putStrLn (result ++ " Test: testExercise72")


test :: IO ()
test = do
  testIsPrime
  testDigitsOnly
  testAllCharsAreDigits
  testDoubleEach
  --testDoubleEachBad
  testExercise71
  testExercise72

