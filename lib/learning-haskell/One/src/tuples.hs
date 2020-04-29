module Tuples where

 {-
  - In tuples we can combine properties of a thing 
  - and treat the result as a single object or record.
  - The properties can have differing data types.
  - There is no schema to enforce consistency from one 
  - to the next record. The resulting tuple becomes a 
  - data type automatically, unless we declare it 
  - explicitly.
  -}
 type X = Integer
 type Y = Integer
 type Point2D = (X, Y) -- "Point2D" is a synonym. 
 

 -- We can store multiple records in the same list 
 -- as long as they have the same data type.
 type Line2D = [Point2D]

 
 -- We can use tuples in functions.
 -- The function below identifies an anonymous tuple.
 -- Anonymous tuples do not have a synonym assigned.
 pairUp :: (Integer, Integer) -> Integer
 -- Note that in the argument definition, parentheses 
 -- are used to indicate that the properties make a tuple.
 -- Otherwise the function would interpret them as 
 -- separate arguments, which causes a compilation failure.
 pairUp (x, y) = x + y

 -- This way we can make endless suppliers:
 nextUp :: (Integer, Integer) -> (Integer, Integer)
 nextUp (x, y) = (y, x + y)

 -- To pull properties out of a tuple we can use 
 -- built-in Haskell functions fst and snd. 
 -- The functions below limit the return type. 
 -- fst and snd do not have that limit.
 takeFirst :: (Integer, Integer) -> Integer
 takeFirst (x, y) = fst (x, y)

 takeSecond :: (Integer, Integer) -> Integer
 takeSecond (x, y) = snd (x, y)

 -- And that means we can translate points:
 translate :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
 translate (x, y) (dx, dy) = (x+dx, y+dy)

 -- Or, using the types we made before:
 translateXY :: (X, Y) -> (X, Y) -> (X, Y)
 translateXY (x, y) (dx, dy) = (x+dx, y+dy)

