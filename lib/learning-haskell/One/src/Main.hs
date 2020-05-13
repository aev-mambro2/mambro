module Main where

-- |This happens anonymously as well, but I like things overt.
import Prelude

-- |System.IO helps us deal with the file system and other communication.
import System.IO (readFile)

-- |Data.Time is imported from the dependency hackage.time.
-- It gives us things like the current time.
import Data.Time (getCurrentTime)

-- |Domain holds data types and functions specific to our knowledge domain.
import qualified Domain (findAccount,findFileLocationForPurpose)

-- |Domain data holds informatino specific to our knowledge domain.
import qualified Domaindata (accounts)

main :: IO ()
main = putStrLn ("The data holds " ++ show (length Domaindata.accounts) ++ " accounts.")

