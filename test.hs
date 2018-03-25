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
**
Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной, которая преобразует значение типа, являющегося представителем Printable, в строковое представление.

Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:

GHCi> toString True
"true"
GHCi> toString False
"false"
GHCi> toString ()
"unit type"

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True   =  "true"
  toString False  =  "false"

instance Printable () where
  toString ()   =  "unit type"
**


**
Сделайте тип пары представителем класса типов Printable, реализованного вами в предыдущей задаче, обеспечив следующее поведение:

GHCi> toString (False,())
"(false,unit type)"
GHCi> toString (True,False)
"(true,false)"
Примечание. Объявление класса типов Printable и представителей этого класса для типов () и  Bool заново реализовывать не надо — они присутствуют в программе, вызывающей ваш код.

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b)  =  "(" ++ toString a ++ "," ++ toString b ++ ")"
**

**
Пусть существуют два класса типов KnownToGork и KnownToMork, которые предоставляют методы stomp (stab) и doesEnrageGork (doesEnrageMork) соответственно:

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
Класса типов KnownToGorkAndMork является расширением обоих этих классов, предоставляя дополнительно метод stompOrStab:

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
Задайте реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp, если переданное ему значение приводит в ярость Морка; вызывает stab, если оно приводит в ярость Горка и вызывает сначала stab, а потом stomp, если оно приводит в ярость их обоих. Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | (doesEnrageGork a, doesEnrageMork a) == (True, False) = stab a
                  | (doesEnrageGork a, doesEnrageMork a) == (False, True) = stomp a
                  | (doesEnrageGork a, doesEnrageMork a) == (False, False) = a
                  | (doesEnrageGork a, doesEnrageMork a) == (True, True) = stomp (stab a)
**
-}

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
**


and, or :: [Bool] -> Bool

and [] = True
and (x:xs) = x && and xs

or [] = False
or (x,xs) = x || or xs

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

**
revWords :: String -> String
let revWords = unwords . map reverse . words
**

**
Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"
Постарайтесь реализовать эту функцию как цепочку композиций, аналогично revWords из предыдущего видео.

import Data.Char

delAllUpper :: String -> String
delAllUpper s = unwords . filter helper . words $ s
  where
    helper s = any (True ==) (map (\c -> isLower c) s)
**


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _  = []
zipWith _ _ []  = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

**
zipWith (+) [1,2] [3,4,5]
[4,6]
**

**
Напишите функцию max3, которой передаются три списка чисел одинаковой длины
и которая возвращает список чисел той же длины, содержащий на k-ой позиции
наибольшее значение из чисел на этой позиции в списках-аргументах.
GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c = zipWith3 (\x y z -> helper x y z) a b c
  where
    helper x y z | x >= y && x >= z = x
                 | y >= x && y >= z = y
                 | otherwise        = z
**

"Генераторы списков"

"Незавершающаяся программа"
let bot = not bot

"Бесконечный список единиц - продуктивная расходящаяся программа"
let ones = 1 : ones

"Ленивая природа хаскеля позволяет работать с продуктивными расходящимяся программами"

nats :: Num t => t -> [t]
let nats n = n : nats (n + 1)

take 10 $ nats 5
[5,6,7,8,9,10,11,12,13,14]

head $ nats 42
[42]

squares :: Num b => [b]
let squares = map (^2) $ nats 1

**
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]

fibStream :: [Integer]
fibStream = fibSt [0] [1]

fibSt [] _ = []
fibSt _ [] = []
fibSt xs ys = xs ++ ys ++ tail (fibSt ys (tail (helper xs ys)))
  where
    helper a b = a ++ (zipWith (+) a b)
**

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate ::  Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys where ys = xs ++ ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x :: iterate f (f x)

"Арифметические последовательности"

[1..10]
enumFromTo 1 10
['a'..'z']

[1,3..10]
enumFromThenTo 1 3 10

[1..]
enumFrom 1

[7,14..]
enumFromThen 7 14

let xs = [1..20]
[x^2 | x <- xs]
"аналог теории множеств - множество икс квадрат где (|) икс принадлежит (<-) xs"

[x^2 | x <- xs, x^2 < 200]
"здесь уже в списке генераторов наложено условие"

[(x,y) | x <- [1,2], y <- [1,2]]
"можно комбинировать и сначало меняется правый генератор заем левый"

[(x,y,z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2]
"пифагорово множество"

**
Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию. Воспользовавшись механизмом генераторов списков, напишите функцию change, которая разбивает переданную ей положительную сумму денег на монеты достоинств из списка coins всеми возможными способами. Например, если coins = [2, 3, 7]:


GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]

Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы [2,2,3] и [2,3,2] — различаются.
Список coins определять не надо.

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change a = [x:xs | x <- coins, a >= x, xs <- change (a - x)]
**

"Правая свертка"

sumList :: [Integer] -> [Integer]
sumList [] = 0
sumList (x:xs) = x + sumList xs

productList :: [Integer] -> [Integer]
productList [] = 1
productList (x:xs) = x * productList xs

concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` foldr f ini xs

sumList' :: [Integer] -> [Integer]
sumList' xs = foldr (+) 0 xs

productList' :: [Integer] -> [Integer]
productList' xs = foldr (*) 1 xs

concatList' :: [[a]] -> [a]
concatList' xs = foldr (++) [] xs

"В бесточечном стиле"
sumList'' :: [Integer] -> [Integer]
sumList'' = foldr (+) 0

productList'' :: [Integer] -> [Integer]
productList'' = foldr (*) 1

concatList'' :: [[a]] -> [a]
concatList'' = foldr (++) []

**
-- посчитать сумму квадратов положительный элементов списка
sumPositiveSquares :: [Integer] -> [Integer]
sumPositiveSquares = foldr (\x s -> if x > 0 then x^2 + s else s) 0

-- с использованием конструкции where let in
sumPositiveSquares' :: [Integer] -> [Integer]
sumPositiveSquares' = foldr f 0 where
  f x s | x >0      = x^2 + s
        | otherwise = s
**

**
Используя функцию foldr, напишите реализацию функции lengthList, вычисляющей количество элементов в списке.

GHCi> lengthList [7,6,5]
3

lengthList :: [a] -> Int
lengthList = foldr (\x s-> 1 + s) 0
**

**
Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:

GHCi> sumOdd [2,5,30,37]
42

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0
**

foldr (-) 5 [1,2,3]
-3

(1 - (2 - (3 - 5)))
-3

foldl (-) 5 [1,2,3]
-1


-}


