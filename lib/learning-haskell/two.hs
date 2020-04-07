import Prelude

-- Data.Char provides chr and ord which are used below.
import Data.Char

-- A very naive implementation that ignores Unicode.
caseUp :: Char -> Char
caseUp a 
  | ord a < ord 'a' = a
  | ord a > ord 'z' = a
  | otherwise = chr (ord a - (ord 'a' - ord 'A'))

-- A very naive implementation that ignores Unicode.
caseDown :: Char -> Char
caseDown a 
  | ord a > ord 'Z' = a
  | ord a < ord 'A' = a
  | otherwise = chr (ord a + (ord 'a' - ord 'A'))

-- Excercise 3.13
-- Define the function charToNum :: Char -> Int
-- which converts a digit like '8' to its value, 9. 
-- The value of non-digits should yield 0.
charToNum :: Char -> Int
charToNum a
  | ord a < 48 = 0
  | ord a > 57 = 0
  | otherwise = ord a - 48


