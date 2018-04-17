module Demo where

type Endo a = a -> a

func :: Endo (Endo Int) -> Int
func x = x (2 +) 1
test = func (\x y -> x y)