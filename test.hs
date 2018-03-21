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

{-

Lists
[] - new List

3 : [] - add 3 to head of empty list

[5,3] == 5 : 3 : []

let cons42 = (42 :)

cons42 [1,2,3]

**
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b c= a : b : c

**
Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. Количество повторов определяется значением второго аргумента этой функции.
nTimes:: a -> Int -> [a]
nTimes a b = helper a b []
  where
    helper a 0 acc = acc
    helper a b acc = helper a (b - 1) (a : acc)


head :: [a] -> a
head [1,2,3] = 1

tail :: [a] -> [a]
tail [1,2,3] -> [2,3]

second :: [a] -> a
let second xs = head (tail xs)
let second = head . tail

let fst' ((,) x y) = x   - сопост авление с образцом для пары в префиксном стиле

let head' ((:) x xs) = x - сопоставление с образцом для списка в префиксном стиле

let head' (x : xs) = x - сопоставление с образцом для списка в инфиксном стиле

second' :: [a] -> a
let second' (_ : xs) = head xs - !!!

second'' :: [a] -> a
let second'' (_ : x : _) = x

sndHead :: [(a, c)] -> c
let sndHead = snd . head

**
Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
oddsOnly :: Integral a => [a] -> [a]
oddsOnly a = filter odd a

last :: [a] -> a
let last (x:[]) = x
let last (_:xs) = last xs

init :: [a] -> [a]
let init [] = error "ERROR!!!"
let init [x] = []
let init (x:xs) = x : init xs

init [1,2,3] -> [1,2]

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip as [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b,bs) (c,cs) = (a,b,c) : zip3 as bs cs
zip3 _ _ _ = []

unzip [(a,b)] -> ([a], [b])
unzip [] = ([],[])
unzip ((x,y):xys) =
  let (xs,ys) = unzip xys
  in (x:xs,y:ys)

**
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse a

**
groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = accum xs [x] []
  where
    accum []     acc     all  = reverse $ acc:all
    accum (x:xs) (z:acc) all | x == z    = accum xs (z:z:acc) all
    accum (x:xs) (z:acc) all | otherwise = accum xs [x] ((z:acc):all)

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (_:xs)      = drop (n-1) xs

splitAt :: Int -> [a] -> [[a],[a]]
splitAt n xs = (take n xs, drop n xs)

(!!) :: [a] -> Int -> a
xs     !! n | n < 0 = error "ERR"
[]     !! _         = error "ERR"
(x:_)  !! 0         = x
(x:xs) !! n         = xs !! (n-1)

filter :: (a -> Bool) -> [a] -> [a] унарный предикат - функция принимающая один аргумент и возвращающая bool
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
  | p x       = dropWhile p xs'
  | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

break :: (a -> Bool) -> [a] -> ([a],[a])
break p = span (not . p)


**
Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")

import Data.Char

readDigits :: String -> (String, String)
readDigits s = span (isDigit) s

**
Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj a b [] = []
filterDisj a b (x:xs)
  | (a x) || (b x) = x : filterDisj a b xs
  | otherwise = filterDisj a b xs

-}

