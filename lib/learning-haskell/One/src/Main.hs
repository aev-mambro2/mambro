module Main where

-- This happens anonymously as well, but I like things overt.
import Prelude

-- System.IO helps us deal with the file system and other communication.
import System.IO (readFile)

-- Data.Time is imported from the dependency hackage.time.
-- It gives us things like the current time.
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  contents <- readFile "Main.hs"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

