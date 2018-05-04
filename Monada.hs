module Demo where

{-
f :: a -> b

 -- Функции с эффектами
f :: a -> b             -- семантика

f :: a -> Maybe b       -- иногда могут завершиться неудачей
f :: a -> [b]           -- могут возвращать много результатов
f :: a -> (Either s) b  -- иногда могут завершиться типизированным исключением
f :: a -> (s,b)         -- могут делать записи в лог
f :: a -> (_> e) b      -- могут читать из внешнего окружения
f :: a -> (State s) b   -- работают с изменяемым состоянием
f :: a -> IO b          -- осуществляют ввод-вывод

Обобщая, получим стрелку Клейсли:
f :: a -> m b


class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b -- произносят bind
  ...
infixl 1 >>=


Prelude> :t return True
return True :: Monad m => m Bool
Prelude> return True :: [Bool]
[True]
Prelude> return True :: Maybe Bool
Just True


toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f = \x -> return (f x) -- or toKleisli f x = return (f x)


**
Введём следующий тип:

data Log a = Log [String] a

Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger

toLogger :: (a -> b) -> String -> (a -> Log b)

которая превращает обычную функцию, в функцию с логированием:

GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6

Далее, определите функцию execLoggers

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c


Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:

GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f xs v = Log [xs] (f v)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers v f g =  Log (xs ++ xss)  v'' where
                     Log xs v' = f v
                     Log xss v'' = g v'
**

**
Функции с логированием из предыдущего задания возвращают в качестве результата значение с некоторой дополнительной информацией в виде списка сообщений. Этот список является контекстом. Реализуйте функцию returnLog

returnLog :: a -> Log a


которая является аналогом функции return для контекста Log. Данная функция должна возвращать переданное ей значение с пустым контекстом.

returnLog :: a -> Log a
returnLog a = Log [] a
**

-- Bind
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

Prelude> (+1) $ (*3) $ (+2) $ 5 -- ((5 + 2) * 3) + 1
22
Prelude> 5 & (+2) & (*3) & (+1) -- ((5 + 2) * 3) + 1
22

-- синтаксический порядок Bind работает точно также как &, с другой стороны похож на функцию fmap
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> :t flip fmap
flip fmap :: Functor f => f a -> (a -> b) -> f b -- fmap не меняет структуру контейнера
Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b     -- bind может изменять сруктуру контейнера


**
Реализуйте фукцию bindLog

bindLog :: Log a -> (a -> Log b) -> Log b

которая работает подобно оператору >>= для контекста Log.

GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs a) f = Log (xs ++ xss) v where
  Log xss v = f a
**


**
Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

Используя return и >>=, определите функцию execLoggersList

execLoggersList :: a -> [a -> Log a] -> Log a

которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a xs = foldl (>>=) (return a) xs
**

-- Дополнительные вспомогательные функции с реализацими по умолчанию в классе типов Monad
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b -- произносят bind

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail s = error s

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c -- оператор "рыбки" - оператор композиции стрелок клейсли
(<=<) f g  x = g x >>= f

-- Представитель класса типов Монад - Identity

newtype Identity a = Identity {runIdentity :: a }
  deriving (Eq,Show)

instance Monad Identity where
  return x = Identity x
  Identity x >>= k = k x -- k - стрелка Клейсли

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

Prelude> runIdentity (wrap'n'succ 3)
4
Prelude> wrap'n'succ 3
Identity {runIdentity = 4}
Prelude> runIdentity $ wrap'n'succ 3
4
Prelude> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ
5
Prelude> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
6
Prelude> 3 & succ & succ & succ
6
Prelude> runIdentity $ return 3 >>= wrap'n'succ >>= wrap'n'succ >>= wrap'n'succ
6


**
Если некоторый тип является представителем класса Monad, то его можно сделать представителем класса Functor, используя функцию return и оператор >>=. Причём, это можно сделать даже не зная, как данный тип устроен.

Пусть вам дан тип

data SomeType a = ...

и он является представителем класса Monad. Сделайте его представителем класса Functor.

instance Functor SomeType where
    fmap f x = (>>=) x (\a -> return (f a))
**

-- Первый закон монад
  return a >>= k   = k a
-- Второй закон монад
  m >>= return     = m
-- Третий закон монад - ассоциативности
  (m >>= k) >>= k' = m >>= (\x -> (k x) >>= k')


Prelude> runIdentity $ (wrap'n'succ 3 >>= wrap'n'succ) >>= wrap'n'succ
6
Prelude> runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= wrap'n'succ)
6
Prelude> runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= \y -> wrap'n'succ y)
6


goWrap0 =
  wrap'n'succ 3 >>=
  wrap'n'succ >>=
  wrap'n'succ >>=
  return

Prelude> runIdentity goWrap0
6

goWrap1 =
  wrap'n'succ 3 >>= (\x ->
  wrap'n'succ x >>= (\y ->
  wrap'n'succ y >>= (\z ->
  return z)))

Prelude> runIdentity goWrap1
6

goWrap2 =
  wrap'n'succ 3 >>= (\x -> -- x := succ 3 ;
  wrap'n'succ x >>= (\y -> -- y := succ x ;
  wrap'n'succ y >>= (\z -> -- z := succ y ;
  return (x,y,z))))        -- return (x,y,z) ;

Prelude> runIdentity goWrap2
(4,5,6)

goWrap3 =
  wrap'n'succ 3 >>= \x ->
  wrap'n'succ x >>= \y ->
  wrap'n'succ y >>
  return (x+y)

Prelude> runIdentity goWrap3
9

-- Do-нотация (синтакисческий сахар для упрощения кодирования)
do { e1 ; e2 }        ==  e1 >> e2
do { p <- e1; e2 }    ==  e1 >>= \p -> e2
do { let v = e1; e2 } ==  let v = e1 in do e2


goWrap4 =
  let i = 3 in
  wrap'n'succ i >>= \x ->
  wrap'n'succ x >>= \y ->
  wrap'n'succ y >>
  return (i,x+y)

Prelude> runIdentity goWrap4
(3,9)

goWrap5 = do  -- порядок в этой конструкции важен - "полноценное императивное программирование"
  let i = 3
  x <- wrap'n'succ i
  y <- wrap'n'succ x
  wrap'n'succ y
  return (i,x+y)

Prelude> runIdentity goWrap5
(3,9)
Prelude> :t goWrap5
goWrap5 :: Identity (Integer, Integer) -- здесь указан тип возвращаемого контейнера


-- Monada Maybe
{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b  -- оператор связывания
  (>>) :: m a -> m b -> m b          -- оператор легковесного связывания
  fail :: String -> m a
 -}


import Prelude hiding (Maybe,Just,Nothing)

data Maybe a = Nothing | Just a
  deriving (Eq,Show)

instance Monad Maybe where
  return x = Just x

  (>>=) (Just x) k = k x -- k-стрелка Клейсли
  (>>=) Nothing _  = Nothing

  (>>) m (Just _) = m
  (>>) _ Nothing  = Nothing

  fail _ = Nothing

**
Рассмотрим язык арифметических выражений, которые состоят из чисел, скобок, операций сложения и вычитания. Конструкции данного языка можно представить следующим типом данных:

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)
Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:

asToken :: String -> Maybe Token

Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char), знаком "+" или "-", открывающейся или закрывающейся скобкой. Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:

GHCi> asToken "123"
Just (Number 123)

GHCi> asToken "abc"
Nothing

Далее, реализуйте функцию tokenize:

tokenize :: String -> Maybe [Token]
Функция принимает на вход строку и если каждое слово является корректным токеном, то она возвращает список этих токенов, завёрнутый в Just. В противном случае возвращается Nothing.

Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию words). Далее, полученный список строк должен быть свёрнут с использованием функции asToken и свойств монады Maybe:

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]

GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]

GHCi> tokenize "1 + abc"
Nothing
Обратите внимание, что скобки отделяются пробелами от остальных выражений!

-- data Token = Number Int | Plus | Minus | LeftBrace | RightBrace    
--     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

import Data.Char

asToken :: String -> Maybe Token
asToken s | (all isDigit s) = Just $ Number $ read s
          | s == "+"        = Just Plus
          | s == "-"        = Just Minus
          | s == "("        = Just LeftBrace
          | s == ")"        = Just RightBrace
          | otherwise       = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = sequence . map asToken $ words input
**


-}