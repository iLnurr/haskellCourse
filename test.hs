module Test (avg, factorial) where

sayHello = putStrLn "Hello world from module Test!"

avg :: Int -> Int -> Int -> Double
avg a b c = ((fromIntegral a) + (fromIntegral b) + (fromIntegral c)) / 3

factorial :: Integer -> Integer
factorial n  | n >= 0   = helper 1 n
             | otherwise = error "arg must be >= 0"
   where
     helper acc 0 = acc
     helper acc n = (helper $! (acc * n)) (n - 1)


{-

Этапы компиляции

1. Синтаксический разбор
2. проверка типов
3. рассахирование -> код на языке КОЕ
4. оптимизация в нескольких циклах
5. кодогенерация
  5.1. код на языке КОЕ -> код СТГ машины
  5.2. код СТГ машины -> код С--
6. генерация либо код для целевой машины либо код LLVM
-}