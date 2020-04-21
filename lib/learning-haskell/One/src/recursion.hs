module Recursion ( 
  divideInt,
  factor,
  fhcif,
  half,
  highest
) where

  {-
   - A factorial is an example of a recursive function.
   - f(x): f(x-1)*x
   - Haskell is good at resolving recursion without getting 
   - lost in stack overflows.
   -}
  
  factor :: Integer -> Integer
  factor x
    | x == 0    = 1
    | x > 0     = factor (x - 1) * x
    | otherwise = 0
  
  
  repeatSubtractToZero :: Integer -> Integer -> Integer
  repeatSubtractToZero x y
    | x > y   = 1 + repeatSubtractToZero (x - y) y
    | x == y  = 1
    | otherwise = 0 
  
  repeatSubtractToOverflow :: Integer -> Integer -> Integer
  repeatSubtractToOverflow x y 
    | x <= 0      = 0
    | y == 0      = x
    | y < 0       = error "2nd argument must exceed zero"
    | (x - y) < 0 = x
    | otherwise   = repeatSubtractToOverflow (x - y) y
  
  divideInt :: Integer -> Integer -> Integer
  divideInt x y 
    | y == 0  = error "cannot divide by 0"
    | x == 0  = 0
    | y == 1  = x
    | y == x  = 1
    | x < 0 && y < 0  = divideInt (-x) (-y)
    | x < 0   = 0 - divideInt (-x) y
    | y > x   = 0
    | y < 0   = 0 - divideInt x (-y)
    | otherwise = 1 + repeatSubtractToZero (x - y) y
  
  half :: Integer -> Integer
  half x = divideInt x 2
  
  highest :: Integer -> Integer -> Integer
  highest x y 
    | x == y  = x
    | x > y   = x
    | otherwise = y
  
  findRemainderInt :: Integer -> Integer -> Integer
  findRemainderInt x y
    | y == 0  = error "cannot divide by 0"
    | x == 0  = 0
    | x == y  = 0
    | y > x   = 0
    | otherwise = repeatSubtractToOverflow (x - y) y
  
  repeatFactorDownToZero :: Integer -> Integer -> Integer -> Integer
  repeatFactorDownToZero x y z
    | z == 0 = 0
    | (findRemainderInt x z) == 0 && (findRemainderInt y z) == 0 = z
    | otherwise = repeatFactorDownToZero x y (z - 1)
  
  -- Find Highest Common Integer Factor
  fhcif :: Integer -> Integer -> Integer
  fhcif x y
   | x == 0  = 0
   | y == 0  = 0
   | x == y  = x
   | otherwise = repeatFactorDownToZero x y (highest(half x)(half y))
  
