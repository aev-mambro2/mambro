module Recursion ( 
  divideInt,
  factor,
  fhcif,
  half,
  highest
) where

{-|
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

lowest :: Integer -> Integer -> Integer
lowest x y
  | x == y  = x
  | x < y   = x
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

-- |Find Highest Common Integer Factor
fhcif :: Integer -> Integer -> Integer
fhcif x y
  | x == 0  = 0
  | y == 0  = 0
  | x == y  = x
  | otherwise = repeatFactorDownToZero x y (lowest x y)

-- |Exercise 7.4
product74 :: [Integer] -> Integer
product74 [] = 1
product74 [x] = x
product74 [x,y] = x * y
product74 (x:xs) = x * (product74 xs)  

testExercise74 :: IO ()
testExercise74 = do
  let input1 = []
  let input2 = [2,3,4]
  let output1 = product74 input1
  let output2 = product74 input2
  let test1 = 1 == output1
  let test2 = 24 == output2
  let result = test1 == True && test2 == True
  let verb = if result == True
         then "Succeeded"
         else "Failed"
  putStrLn (verb ++ " test: testExercise74")

-- |Exercise 7.5
and75 :: [Bool] -> Bool
and75 [] = True
and75 (x:xs) = x && (and75 xs) 

or75 :: [Bool] -> Bool
or75 [] = False
or75 (x:xs) = x || (or75 xs)

testExercise75 :: IO ()
testExercise75 = do
  let x = [True, True, True]
  let y = [True, False, False]
  let z = [False, False, False]
  let a = and75 x
  let b = and75 y
  let c = and75 z
  let d = or75 x
  let e = or75 y
  let f = or75 z
  let t = a == True && b == False && c == False && d == True && e == True && f == False
  let v = if t == True
            then "Succeeded"
            else "Failed"
  putStrLn (v ++ " test: testExercise75") 


test :: IO ()
test = do
  testExercise74
  testExercise75


