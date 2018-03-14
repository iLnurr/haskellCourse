module Demo where

class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int

class Bounded a where
  minBound, maxBound :: a

class SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a