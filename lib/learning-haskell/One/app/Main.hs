module Main where

import Recursion

main :: IO ()
main = do
  let a = (Recursion.fhcif 45 60)
  let isGood = (15 == a)
  print isGood
