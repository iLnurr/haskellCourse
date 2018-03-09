module Demo where

class EqI a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool

instance EqI Bool where
  True  === True   =  True
  False === False  =  True
  _     === _      =  False

  x /== y  = not (x === y)
