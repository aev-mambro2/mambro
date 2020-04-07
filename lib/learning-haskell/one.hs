{-##############################################

A Haskell script
is when everything is considered a program 
except or those lines marked to be a comment.

##############################################-}


doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x

{-
 - The Integer data type can hold very large numbers. 
 - The Int data type is limited from -2^31 to +2^31. 
 - Larger values will yield 0. Integer will keep going.
 -}

raise :: Integer -> Integer -> Integer
raise a b = a^b

square :: Integer -> Integer
square n = raise n 2

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

{-
 - The Java compare() method returns a negative integer 
 - if a < b, a positive integer if a > b, and 0 if they equal.
 - It looks like Haskell already has a built-in compare
 - that returns LT, GT, or EQ, respectively. So we name ours.
 -}

vergelijk :: Integer -> Integer -> Integer
vergelijk a b 
  | a < b = (-1)
  | a > b = 1
  | otherwise = 0

{- 
 - Note: if our module defines a function that already 
 - exists in Haskell, Hugs will ignore ours in favor of 
 - the built-in function. 
 -}
