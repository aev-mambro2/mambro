module Main where

-- This happens anonymously as well, but I like things overt.
import Prelude

-- System.IO helps us deal with the file system and other communication.
import System.IO (readFile)

-- Data.Time is imported from the dependency hackage.time.
-- It gives us things like the current time.
import Data.Time (getCurrentTime)

-- The Recursion module contains a few training exercises 
-- I intend to test in this Main module.
-- import Recursion

main :: IO ()
main = do
  --let a = (fhcif 45 60)
  --let isGood = (15 == a)
  contents <- readFile "stack.yaml"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  putStrLn (show time)
