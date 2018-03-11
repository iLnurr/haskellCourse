module Demo where

class EqI a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x /== y  = not (x === y)
  x === y  = not (x /== y)

instance EqI Bool where
  True  === True   =  True
  False === False  =  True
  _     === _      =  False

instance (EqI a, EqI b) => EqI (a, b) where
  p1 === p2  =  fst p1 === fst p2 && snd p1 === snd p2

class (EqI a) => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare :: a -> a -> Ordering