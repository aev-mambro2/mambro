module Test where

import Recursion

main :: IO ()
main = do
  let a = (fhcif 45 60)
  let isGood = (15 == a)
  print (isGood)
