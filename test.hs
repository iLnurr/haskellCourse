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
(((5 - 1) - 2) - 3)
-1

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini []     = ini
foldl f ini (x:xs) = foldl f (f ini x) xs
-- foldl не рекомендуеться использовать в хаскеле из-за крайней неэффективности - если будет список с 1000000 значений - то в результате работы foldl образуется огромное отложенное вычисление (1000000 применений функциии f) 'огромный санк'

--рекомендуется использовать строгую версию левой свертки
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f ini []     = ini
foldl' f ini (x:xs) = ini' `seq` foldl' f ini' xs
  where ini' = f ini x
  {- здесь вычисления форсированы (с помощью комбинатора seq) и значит в конце не будет накоплено огромное отложенное вычисление -}

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x b -> p x || b) False

**
За один проход по списку вычислить и сумму и произведение
foldr (\x (s,p) -> (x + s, x * p)) (0,1) [1,2,3,4]
(10,24)
**

**
Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.

GHCi> meanList [1,2,3,4]
2.5

Постобработка считается допустимой, то есть предполагаемая реализация функции meanList имеет вид

meanList = someFun . foldr someFoldingFun someIni

meanList :: [Double] -> Double
meanList = (\(p1,p2) -> p1 / p2) . foldr (\x (p1,p2) -> (x + p1, p2 + 1)) (0,0)
**

**
Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"

evenOnly :: [a] -> [a]
evenOnly = (foldr (\(a,b) y -> if (even a) then b:y else y) []) . foo
foo xs = zipWith (,) [1..(length xs)] xs
**

**
Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция evenOnly позволяла работать и с бесконечными списками.

То есть, например, запрос на первые три элемента бесконечного списка, возвращаемого этой функцией, примененной к списку всех натуральных чисел, должен завершаться:

GHCi> take 3 (evenOnly [1..])
[2,4,6]


evenOnly' :: [a] -> [a]
evenOnly' xs = bar xs 1
bar [] _ = []
bar (x:xs) c = if (even c) then x:(bar xs (c+1)) else (bar xs (c+1))
**

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ [] = error "foldr1: EmptyList"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "foldl1: EmptyList"

maximum :: (Ord a) => [a] -> a
maximum  = foldl1 max

**
Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.

lastElem :: [a] -> a
lastElem = foldl1 undefined

lastElem :: [a] -> a
lastElem = foldl1 (flip const)
**

**
foldl f ini [1,2,3] ~>> ((ini `f` 1) `f` 2) `f` 3
**

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f ini [] = [ini]
scanl f ini (x:XS) = ini : scanl f (ini `f` x) xs

**
scanl (*) 1 [1..10]
[1,1,2,6,24,120,720,5040,40320,362880,3628800]
**

facs :: (Num a, Enum a) => a
facs = scanl (*) 1 [1..]

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

take 15 . partialSums . map (**(-1)) $ facs

**
foldr f ini [1,2,3] ~>> 1 `f` (2 `f` (3 `f` ini))
**

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ ini [] = [ini]
scanr f ini (x:xs) = (x `f` q) : qs
                        where qs@(q:_) = scanr f ini xs

**
scanr (+) 0 [1,2,3]
[6,5,3,0]
**

unfold :: (b -> (a,b)) -> b -> [a]
unfold f ini = let (x,ini') = f ini in
  x : unfold f ini'

**
iterate f x == [x, f x, f (f x), f (f (f x)), ...]

iterate :: (a -> a) -> a -> [a]
**

iterate f = unfold (\x -> (x,f x))

Maybe

find :: (a -> Bool) -> [a] -> Maybe a
lookup :: Eq a => a -> [(a,b)] -> Maybe b


unfoldr :: (b -> Maybe(a,b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
  helper (Just (x,ini')) = x : unfoldr f ini'
  helper Nothing        = []

unfoldr (\x -> if x == 10 then Nothing else Just (x, x + 2)) 0
[0,2,4,6,8]

unfoldr () 'a'

**
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g = undefined
GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"

revRange :: (Char,Char) -> [Char]
revRange (x,y) = reverse $ unfoldr (\ini -> if (ini >= x && ini <= y) then Just (ini, succ ini) else Nothing) x
**

-}


{-
import Prelude hiding (Bool,True,False)

data Bool = True | False

--имя типа = конструкторы данных разделенных `|`

alwaysTrue :: Int -> Bool
alwaysTrue n = True

data B = T | F deriving (Show,Eq,Read,Enum)
-- deriving - автоматическая реализация производных представителей (в данном случае представиель класса В становится представителем класса Show)

not' :: B -> B
not' T = F

Prelude> :set -fwarn-incomplete-patterns

not'' :: B -> B
not'' T = F
not'' F = T

**
Тип данных Color определен следующим образом

data Color = Red | Green | Blue
Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.

GHCi> show Red
"Red"

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
**

intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar _ = 'N'

isz :: Char - > Bool
isz 'z' = True
isz _   = False

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False

**
Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.

GHCi> charToInt '0'
0
GHCi> charToInt '9'
9

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
**

**
Определите (частичную) функцию stringToColor, которая по строковому представлению цвета как в прошлой задаче возвращает исходный цвет.


GHCi> stringToColor "Red"
Red

data Color = Red | Green | Blue

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue
**

Сопоставление с образцом 3 следующих состояния:
 1 - Успешное сопоставление с образцом.
 2 - Неуспешное сопоставление с образцом, тогда переходим к следующему уравнению
 3 -  Расходящееся вычисление, возникшее в результате сопоставления с образцом.

 Пункт 2 не эквивалентен пункту 3. Потому что неуспешное сопоставление с образцом, в отличие от пункта 3, совсем не всегда приводит к невозможности вычислить функцию, т.к. сопоставление со следующим образцом может произойти успешно, и функция вычислится.

foo 1 2 = 3
foo 0 _ = 5

GHCi> foo 0 undefined <-- расходимости нет потому что вотрой аргумент не вычисляется (ленивые вычисления)
5

GHCi> foo undefined 0 <-- расходимость потому что по первому аргументу производятся энергичные вычисления
*** Exception: Prelude.undefined

GHCi> foo 2 2 <-- расходимость потому что паттерна не существет
*** Exception: non-exhaustive pattern

**
Пусть определены следующие функции:

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

Выберите варианты вызовов этих функций, при которых сопоставление с образцом будет осуществлено успешно.

emptyOrSingleton True undefined
isEqual (undefined, undefined) (undefined, undefined)
emptyOrSingleton False undefined
**

**
Тип LogLevel описывает различные уровни логирования.
data LogLevel = Error | Warning | Info

Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
GHCi> cmp Error Warning
GT
GHCI> cmp Info Warning
LT

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Info _  = LT
cmp Warning Info = GT
cmp Warning Error = LT
**

--Обычно сопсставления с образцом производится в левой части, но если нужно в правой то используется конструкция `case of`

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering

lessThanError :: LogLevel -> Bool
lessThanError lvl =
  case cmp lvl Error of
    LT -> True
    _  -> False


**
Пусть объявлен следующий тип данных:

data Result = Fail | Success


И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result,Int)
возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения, либо строку "Fail: N" в случае неудачи, где N — код ошибки.

processData :: SomeData -> String
processData somedata =
  case doSomeWork somedata of
    (Success,_) -> "Success"
    (Fail,x)    -> "Fail: " ++ show x
**

data Point = Pt Double Double deriving Show

origin :: Point
origin = Pt Double Double

distanceToOrigin :: Point -> Double
distanceToOrigin (Pt x y) = sqrt (x^2 + y^2)

**
Реализуйте функцию distance, возвращающую расстояние между двумя точками.

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
**

rootsOld :: Double -> Double -> Double -> (Double,Double)
rootsOld a b c = (x1,x2) where
  x1 = helper (-d)
  x2 = helper (d)
  helper x = (-b + x) / (2 * a)
  d = sqrt discr
  discr = b^2 - 4 * a * c

data Roots = Roots Double Double | None
  deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c
  | discr >=0 = Roots x1 x2
  | otherwise = None
  where
    x1 = helper (-d)
    x2 = helper (d)
    helper x = (-b + x) / (2 * a)
    d = sqrt discr
    discr = b^2 - 4 * a * c

**
Определим тип фигур Shape:

data Shape = Circle Double | Rectangle Double Double
У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b. Реализуйте функцию area, возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b
**

**
В одном из прошлых заданий мы встречали тип Result и функцию doSomeWork:

data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)

Функция doSomeWork возвращала результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха. Такое определение функции не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки.

Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код ошибки только в случае неудачи. Для этого необходимо определить тип Result'. Кроме того, определите instance Show для Result' так, чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.

Программирование — Напишите программу. Тестируется через stdin → stdout

data Result' = Fail' Int | Success'

instance Show Result' where
    show Success' = "Success"
    show (Fail' erNo) = "Fail: " ++ show erNo

doSomeWork' :: SomeData -> Result'
doSomeWork' x =
  case doSomeWork x of
    (Success, _) -> Success'
    (_, er) -> Fail' er
**

**
Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.

data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Circle a) = False
isSquare (Rectangle a b) = if a == b then True else False
**

-- неопровержимые образцы
-- ленивый образец ~

fromMaybe (Just x) = x
fromMaybe Nothing  = error "!!!"

fromMaybe' ~(Just x) = x
fromMaybe' Nothing  = error "!!!" -- сюда никогда не доберемся!



(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b.d)
(***) f g p = (f $ fst p, g $ snd p)

Prelude> succ *** pred (5,5)
(6,4)
Prelude> const 1 *** const 2 $ (5,5)
(1,2)
Prelude> const 1 *** const 2 $ (undefined,undefined)
(1,2)
Prelude> const 1 *** const 2 $ undefined
(1,2)

-- пример на сопоставлении с образцом
(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b.d)
(***) f g (x,y) = (f x, g y)

Prelude> succ *** pred (5,5)
(6,4)
Prelude> const 1 *** const 2 $ (5,5)
(1,2)
Prelude> const 1 *** const 2 $ (undefined,undefined)
(1,2)
Prelude> const 1 *** const 2 $ undefined
*** Exception Prelude.undefined

-- пример на сопоставлении с ленивым образцом
(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b.d)
(***) f g ~(x,y) = (f x, g y)

Prelude> succ *** pred (5,5)
(6,4)
Prelude> const 1 *** const 2 $ (5,5)
(1,2)
Prelude> const 1 *** const 2 $ (undefined,undefined)
(1,2)
Prelude> const 1 *** const 2 $ undefined
(1,2)

-- Синтаксис записей
data Person' = Person' String String Int

firstName' :: Person' -> String
firstName' (Person x _ _) = x

lastName' :: Person' -> String
lastName (Person' _ y _) = y

age' :: Person' -> Int
age' (Person' _ _ z) = z

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq)

Prelude> :t firstName
firstName :: Person -> String

Prelude> let john = Person "John" "Smith" 33
Prelude> age john
33
Prelude> lastName john
"Smith"
Prelude> john
Person {firstName = "John", lastName = "Smith", age = 33}

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

Prelude> john & age
33

Prelude>f $ g $ h $ x
Prelude>x & h & g & f -- Same? because of & is left associative

**
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:

    timestamp — время, когда произошло событие (типа UTCTime);
    logLevel — уровень события (типа LogLevel);
    message — сообщение об ошибке (типа String).

Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, и функцию logEntryToString, возвращающую текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>



Для преобразование типа UTCTime в строку используйте функцию timeToString.

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry t l s) = (timeToString t) ++ ": " ++ (logLevelToString l) ++ ": " ++ s
**

Prelude> let xavier = Person {age = 40,firstName = "Phideaux",lastName = "Xavier"} -- alternative way

Prelude> let unknownBill = Person {firstName = "Bill"}
<interactive>:9:19: Warning:
    Fields of ‘Person’ not initialised: lastName, age
    In the expression: Person {firstName = "Bill"}
    In an equation for ‘unknownBill’:
        unknownBill = Person {firstName = "Bill"}

Prelude> unknownBill
Person {firstName = "Bill", lastName = "*** Exception: <interactive>:9:19-45: Missing field in record construction lastName

Prelude> firstName unknownBill
"Bill"

updateAge :: Int -> Person -> Person
updateAge newAge person = person {age = newAge} -- здесь создается новый объект  Person

**
Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.

data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = (lastName person1)}
**

name :: Person -> String
name person = firstName person ++ " " ++ lastName person

name' :: Person -> String
name' (Person fn ln _) = fn ++ " " ++ ln

name'' :: Person -> String
name'' (Person {lastName = ln,firstName = fn}) = fn ++ " " ++ ln

**
Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", то после применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = Person {firstName = name, lastName = (lastName p), age = (age p)} where
                    name = if length (firstName p) < 2
                           then firstName p
                           else (first_char):'.':[]
                    first_char = head (firstName p)
**

-- Типы с параметрами

data CoordD = CoordD Double Double

data CoordI = CoordI Int Int

data Coord a = Coord a a deriving Show -- конструктор типа применяется к типам порождая тип , конструктор данных применяется к данным порождая выражения

Prelude> :t Coord
Coord :: a -> a -> Coord a
Prelude> Coord (3::Int) (4::Int)
Coord 3 4
Prelude> :t Coord (3::Int) (4::Int)
Coord (3::Int) (4::Int) :: Coord Int
Prelude> :t Coord (3.5::Double) (4.3::Double)
Coord (3.5::Double) (4.3::Double) :: Coord Double

**
Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами, и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord a1 a2) (Coord b1 b2) = sqrt ((a1 - b1)^2 + (a2 - b2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord a1 a2) (Coord b1 b2) = abs (a1 - a2) + abs (b1 - b2)
**

**
Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат. Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты. Один из углов этой ячейки имеет координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.

Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, и функцию getCell, которая принимает координату точки и возвращает номер ячейки в которой находится данная точка. В качестве первого аргумента обе эти функции принимают ширину ячейки.

data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = (Coord centerX centerY) where
                                 d = width/2
                                 centerX = width * fromIntegral x + d
                                 centerY = width * fromIntegral y + d

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = (Coord numX numY) where
                                numX = floor (x / width)
                                numY = floor (y / width)
**


-- Стандартные параметризованные типы

twice :: a -> [a]  -- конструктор типов записанный в миксфиксной форме
twice x => [x,x]

twice :: a -> [] a -- конструктор типов записанный в префиксной форме
twice x => [x,x]

thrice :: a -> (,,) a a a -- конструктор типов записанный в префиксной форме
thrice x => (,,) x x x    -- конструктор данных  записанный в префиксной форме

id' :: (->) a a -- функциональная стрелочка также является конструктором параметризованного типа
id' x = x

data Maybe a = Nothing | Just a -- тип данных Maybe является примером типа с параметром

data Either a b = Left a | Right b

**
Реализуйте функцию, которая ищет в строке первое вхождение символа, который является цифрой, и возвращает Nothing, если в строке нет цифр.

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs
**

**
Реализуйте функцию findDigitOrX, использующую функцию findDigit (последнюю реализовывать не нужно). findDigitOrX должна находить цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'. Используйте конструкцию case.

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX s =
  case findDigit s of
    Nothing -> 'X'
    Just res -> res
**

**
Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1. Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и наоборот (вторая функция отбрасывает все элементы списка, кроме первого).

maybeToList :: Maybe a -> [a]
maybeToList a =
  case a of
    Nothing -> []
    Just a -> [a]


listToMaybe :: [a] -> Maybe a
listToMaybe a = if length a >= 1 then Just (head a) else Nothing
**

-- Виды (kind)

Prelude> :type 'c'
'c' :: Char
Prelude> :kind Char
Char :: *
Prelude> :k Int
Int :: *
Prelude> :k Maybe
Maybe :: * -> *
Prelude> :k Maybe Int
Maybe Int :: *
Prelude> :k Maybe Int Int

<interactive>:1:1:
    ‘Maybe’ is applied to too many type arguments
    In a type in a GHCi command: Maybe Int Int
Prelude> :k Either Int Int
Either Int Int :: *
Prelude> :k []
[] :: * -> *
Prelude> :k [] Int
[] Int :: *
Prelude> :k [Int]
[Int] :: *
Prelude> :k Int -> []

<interactive>:1:8:
    Expecting one more argument to ‘[]’
    Expected a type, but ‘[]’ has kind ‘* -> *’
    In a type in a GHCi command: Int -> []
Prelude> :k Int -> [] Int
Int -> [] Int :: *
Prelude> :k (,)
(,) :: * -> * -> *
Prelude> :k (,,)
(,,) :: * -> * -> * -> *
Prelude> :k (->)
(->) :: * -> * -> *

-- Флаги строгости (аргумент вычисляется в момент создания - "энергично")

data CoordLazy a = CoordLazy a a
  deriving Show

data CoordStrict a = CoordStrict !a !a
  deriving Show

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x

Prelude> getXLazy (CoordLazy 3 5)
3
Prelude> getXStrict (CoordStrict 3 5)
3
Prelude> getXLazy (CoordLazy 3 undefined)
3
Prelude> getXStrict (CoordStrict 3 undefined)
*** Exception: Prelude.undefined

Prelude> 2 :+ 5 -- комплексное число (в этих типах данных используются флаги строгости)
2 :+ 5

-- data Complex a = !a :+ !a
-- data Ratio a = !a :% !a


-- Рекурсивные типы данных

data List a = Nil | Cons a (List a) deriving Show

Prelude> :t Nil
Nil :: List a
Prelude> :t Cons 'z' Nil
Cons 'z' Nil :: List Char
Prelude> :t Cons 'y' (Cons 'z' Nil)
Cons 'y' (Cons 'z' Nil) :: List Char
Prelude> let yz = Cons 'y' (Cons 'z' Nil)
Prelude> yz
Cons 'y' (Cons 'z' Nil)
Prelude> :t yz
yz :: List Char
Prelude> Cons 'x' yz
Cons 'x' (Cons 'y' (Cons 'z' Nil))

**
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, что существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : (fromList b)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)
**

**
Рассмотрим еще один пример рекурсивного типа данных:

data Nat = Zero | Suc Nat
Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее. Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.

Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add a b = toNat ((fromNat a) + (fromNat b))

mul :: Nat -> Nat -> Nat
mul a b = toNat ((fromNat a) * (fromNat b))

fac :: Nat -> Nat
fac a = toNat (factorial (fromNat a))

factorial :: Integer -> Integer
factorial n  | n >= 0   = helper 1 n
             | otherwise = error "arg must be >= 0"
   where
     helper acc 0 = acc
     helper acc n = (helper $! (acc * n)) (n - 1)
**

**
Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node (Tree a) (Tree a)

Реализуйте функцию height, возвращающую высоту дерева, и функцию size, возвращающую количество узлов в дереве (и внутренних, и листьев). Считается, что дерево, состоящее из одного листа, имеет высоту 0.

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (max (height a) (height b))

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = 1 + size a + size b
**

**
Теперь нам нужно написать функцию avg, которая считает среднее арифметическое всех значений в дереве. И мы хотим, чтобы эта функция осуществляла только один проход по дереву. Это можно сделать при помощи вспомогательной функции, возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.

data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf l) = (1,l)
    go (Node l r) =
        let (a,b) = go l
            (e,d) = go r
        in (a + e , b + d)
**

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
  deriving (Show,Eq)

expr1 = Val 2 :+: Val 3 :*: 4
expr2 = (Val 2 :+: Val 3) :*: 4

eval :: Expr -> Int
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

-- Синонимы и обертки типов
allUpper :: String -> Bool
allUpper = all isUpper

type IntegerList = [Integer]

sumSquares :: IntegerList -> Integer
sumSquares = foldr1 (+) . map (^2)

type AssocList k v = [(k,v)] -- параметризованный тип

lookup' :: Eq k -> AssocList k v -> Maybe v
lookup' _ []  = Nothing
lookup' key ((x,y):xys)
  | key == x = Just y
  | otherwise = lookup' key xys

import qualified Data.Map as Map

type IntMap = Map.Map Int

Prelude Map> :k Map.Map
Map.Map :: * -> * -> *
Prelude Map> :k Map.Map Int
Map.Map Int :: * -> *

newtype IntList = IList [Int] deriving Show -- обертка типа по сравнению с конструктором данных - 1) имеет ровно один (!) конструктор и более эффективную реализацию, 2) более ленив
                                            -- контейнер используемый во время разработки, для контроля над типами
example = IList [1,2]
Preload> example
IList [1,2]

data IntList' = IList' [Int] deriving Show

ignore' :: InList' -> String
ignore' (IList' _) = "Hello" -- использует сопоставление с образцом и при вызове функции с undefined - будет брошено исключение

ignore :: InList -> String
ignore' (IList _) = "Hello" -- использует сопоставление с образцом  и при вызове функции с undefined - будет выведена строка, так как обертка типоа более ленива

newtype Identity a = Identity {runIdentity :: a} deriving (Eq,Ord)

*Demo> :k Identity -- про конструктор типа
Identity :: * -> *
*Demo> :t Identity -- про конструктор данных
Identity :: a -> Identity a
*Demo> :t runIdentity
runIdentity :: Identity a -> a

-- Monoid - класс типов, реализация математического определения Monoid

class Monoid a where
  mempty :: a             -- нейтральный элемент
  mappend :: a -> a -> a  -- операция

  mconcat :: [a] -> a     -- свертка
  mconcat = foldr mappend mempty

{- законы
mempty  'mappend' x = x -- "Левый нейтральный элемент"
x 'mappend' mempty = x  -- "Правый нейтральный элемент"
(x 'mappend' y) 'mappend' z = x 'mappend' (y 'mappend' z) -- "Требование к ассоциативности операции"
 -}

instance Monoid [a] where
  mempty = []
  mappend == (++)


Prelude> mconcat [[1,2],[],[3,4,5]]
[1,2,3,4,5]

-- числа - дважды Monoid - относительно сложения (mempty=0) и умножжения (mempty=1)

для того чтобы числа использовать как моноиды - нужно задать упаковать в обертку newtype

newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

**
Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    Xor x `mappend` Xor y | x == True && y == False = Xor True
                          | x == False && y == True = Xor True
                          | otherwise = Xor False
**

-- Для пар и кортежей - они являются моноидами если их элементы являются моноидами

instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (x1,x2) `mappend` (y1,y2) =
                (x1 `mappend` y1, x2 `mappend` y2)


instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq,Show,Ord,Read)

instance Monoid (First a) where
  mempty = First Nothing
  First Nothing `mappend` r = r
  l `mappend` _             = l

Prelude> mconcat [First Nothing, First (Just 3), First (Just 5)]
First {getFirst = Just 3}
Prelude> mconcat $ map First [Nothing, (Just 3), (Just 5)]
First {getFirst = Just 3}
Prelude> getFirst $ mconcat $ map First [Nothing, (Just 3), (Just 5)]
Just 3
Prelude> let firstConcat = getFirst . mconcat . map First
Prelude> firstConcat [Nothing, Just 3, Just 5]
Just 3

-- Эндоморфизмы как моноиды
Prelude> :t [(*2),(+5),(^2)]
[(*2),(+5),(^2)] :: Num a => [a -> a]

Prelude> zipWith ($) [(*2),(+5),(^2)] [1,2,3]
[2,7,9]

newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
  mempty = Endo id
  Endo f `mappend` Endo g = Endo (f . g) -- композиция функций


-- Functor - "поднимает функцию в контейнер"

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

Prelude> :t map
map :: (a -> b) -> [a] -> [b]
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> map succ [1,2,3]
[2,3,4]
Prelude> fmap succ [1,2,3]
[2,3,4]

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just a) = Just (f a)

Prelude> fmap (*2) Nothing
Nothing
Prelude> fmap (*2) (Just 21)
Just 42

**
Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве:

data Point3D a = Point3D a a a deriving Show

GHCi> fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8

instance Functor Point3D where
    fmap f (Point3D a b c)= Point3D (f a) (f b) (f c)
**

**
Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)

GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)

instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)
**

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

testTree = Branch (Leaf 2) 3 (Leaf 4)

Prelude> succ `fmap` [1,2,3]
[2,3,4]
Prelude> succ <$> [1,2,3]
[2,3,4]
Prelude> :i <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
        -- Defined in ‘Data.Functor’
infixl 4 <$>
Prelude> (+5) <$> succ <$> [1,2,3]
[7,8,9]

Prelude> :i <$
class Functor (f :: * -> *) where
  ...
  (<$) :: a -> f b -> f a
        -- Defined in ‘GHC.Base’
infixl 4 <$
Prelude> 42 <$ [1,2,3]
[42,42,42]
Prelude>

**
Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

GHCi> words <$> Leaf Nothing
Leaf Nothing

GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])

instance Functor Tree where
    fmap f (Leaf m) = Leaf (fmap f m)
    fmap f (Branch l x r) = Branch (fmap f l) (fmap f x) (fmap f r)
**

Prelude> :k (,)
(,) :: * -> * -> *
Prelude> :k (,) Int
(,) Int :: * -> * -- kind подходит для реализации представителя класса типов Functor - так как второй тип мы связали с определенным типом (Int)

instance Functor ((,) s) where
  fmap g (x,y) = (x, g y)

-- (a -> b) -> (s,a) -> (s,b)

Prelude> fmap succ (1,'A')
(1,'B')

instance Functor (Either e) where
  fmap _ (Left x)  = Left x
  fmap f (Right y) = Right (g y)

-- (a -> b) - > Either e a -> Either e b
Prelude> fmap (+3) $ Right 5
Right 8
Prelude> fmap (+3) $ Left 5
Left 5
Prelude> fmap (+3) $ Left "AAA"
Left "AAA"

Prelude> :k (->)
(->) :: TYPE q -> TYPE r -> *
Prelude> :k (->) Int
(->) Int :: * -> *

instance Functor ((->) e) where
  fmap = (.)

-- (a -> b) -> (e -> a) -> (e -> b) - композиция функций
Prelude> :t fmap length tail --
fmap length tail :: [a] -> Int
Prelude> fmap length tail "ABC"
2

**
Определите представителя класса Functor для типов данных Entry и Map. Тип Map представляет словарь, ключами которого являются пары:

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

В результате должно обеспечиваться следующее поведение: fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.

GHCi> fmap (map toUpper) $ Map []
Map []

GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
    fmap f (Map arr) =  Map (map g arr) where g = fmap f
**

{-Законы функторов. Их 2:
(1)  fmap id xs = id xs
(2)  fmap (f . g) xs = (fmap f . fmap g) xs
fmap гарантированно не меняет структуру контейнера.
-}

Prelude> fmap id [1,2,3]
[1,2,3]
Prelude> fmap ((+1) . (^2)) [1,2,3]
[2,5,10]



-}