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

**
Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. Количество повторов определяется значением второго аргумента этой функции.
nTimes:: a -> Int -> [a]
nTimes a b = helper a b []
  where
    helper a 0 acc = acc
    helper a b acc = helper a (b - 1) (a : acc)
**

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
**

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
**

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
**

 **
 Напишите реализацию функции qsort. Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара: для какого-то элемента x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x, и потом запускаться рекурсивно на обеих частях.

 GHCi> qsort [1,3,2,5]
 [1,2,3,5]

 qsort :: Ord a => [a] -> [a]
 qsort [] = []
 qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>=x) xs)
 **

map :: (a -> b) -> [a] -> [b]
map _ [] -> []
map f (x:xs) = f x : map f xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat (map f xs)

**
Prelude> concatMap (\x -> [x,x,x]) "ABCD"
"AAABBBCCCDDD"
**

**
Напишите функцию squares'n'cubes, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.
GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes xs = concatMap (\x -> [x * x, x * x * x]) xs
**

**
Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.

perms :: Eq a => [a] -> [[a]]
perms [] = []
perms xs@(x:xs') = concatMap (\x -> helper x (filter (x /=) xs)) xs
  where
    helper x []     = [[x]]
    helper x [y]  = [[x,y],[y,x]]
    helper x [y,z]  = [[x,y,z],[x,z,y],[y,x,z],[y,z,x],[z,x,y],[z,y,x]]
    helper x (y:ys) = map (\a -> x:a) (helper y ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertElem x) (perms xs) where
			insertElem x [] = [[x]]
			insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)


-}

