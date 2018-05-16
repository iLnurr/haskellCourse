module IO where

promptLine :: String -> IO String
promptLine prompt = do
  putStrLn "What is your name?"
  putStr prompt
  name <- getLine
  if null name then promptLine prompt else return name

main' :: IO ()
main' = do
  name <- promptLine "Name: "
  putStrLn $ "Nice to meet you, " ++ name ++ "!"


{-

**
На этом шаге вы будете работать с монадой IO, а значит, ваша программа будет взаимодействовать с операционной системой. Чтобы тестирующая система смогла оценить вашу программу, пожалуйста, используйте только функции, осуществляющие ввод/вывод на терминал: getChar, putChar, putStr, putStrLn, getLine. Все эти функции уже будут находиться в области видимости, так что вам не следует их импортировать. По той же причине, главная функция вашей программы будет называться не main, а main' (со штрихом).

Напишите программу, которая будет спрашивать имя пользователя, а затем приветствовать его по имени. Причем, если пользователь не ввёл имя, программа должна спросить его повторно, и продолжать спрашивать, до тех пор, пока пользователь не представится.

Итак, первым делом, программа спрашивает имя:

What is your name?
Name:

Пользователь вводит имя и программа приветствует его:

What is your name?
Name: Valera
Hi, Valera!


Если же пользователь не ввёл имя, необходимо отобразить точно такое же приглашение ещё раз:
What is your name?
Name:
What is your name?
Name:
What is your name?
Name: Valera
Hi, Valera!


Пожалуйста, строго соблюдайте приведенный в примере формат вывода. Особое внимание уделите пробелам и переводам строк! Не забудьте про пробел после Name:, а также про перевод строки в самом конце (ожидается, что вы будете использовать putStrLn для вывода приветствия пользователя).

promptLine :: String -> IO String
promptLine prompt = do
  putStrLn "What is your name?"
  putStr prompt
  name <- getLine
  if null name then promptLine prompt else return name

main' :: IO ()
main' = do
  name <- promptLine "Name: "
  putStrLn $ "Hi, " ++ name ++ "!"
**

-}

{-
-- newtype IO a = IO (RealWorld -> (RealWorld, a))
type IO a = RealWorld -> (RealWorld, a) -- Псевдо-код

-- return :: a -> IO a
return :: a -> RealWorld -> (RealWorld, a)

-- (>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) :: (RealWorld -> (RealWorld, a)) -> (a -> RealWorld -> (RealWorld, b)) -> RealWorld -> (RealWorld, b)

instance Monad IO where
  return a = \w -> (w,a) -- w - world

  (>>=) m k = \w -> case m w of (w',a) -> k a w' -- k - Kleisli, w and w' - world, m - monada
  -- побочный эффект первого ввода-вывода m и действе второго ввода-вывода в стрелке клейсли k - должны выполняться ровно один раз
  -- m должен выполняться до k

-}

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n' then
    return []
  else do
    cs <- getLine'
    return (c:cs)

{-

*IO> getLine'
qwerty
"qwerty"
*IO> :t putChar
putChar :: Char -> IO ()


-}

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs  -- >> - оператор монадического связывания bind (поверхностная версия)

{-

*IO> putStr' "ASDF"
ASDF
-}

import Control.Monad
{-
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

*IO> sequence_ [Just 1, Just 2]
Just ()
*IO> sequence_ [Just 1, Just 2,Nothing]
Nothing
*IO> sequence_ [[1,2],[3,4,5,6]]
[(),(),(),(),(),(),(),()] -- 8 вычислений!!
-}

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

{-
*IO> sequence_ [putChar 'a', putChar 'b']
ab
*IO> sequence_ $ map putChar "ab"
ab
-}

{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

*IO> mapM_ (\x -> [x,x]) "ABC"
[(),(),(),(),(),(),(),()] -- не 6 вычислений, а 8 - потому что 2*2*2, а не 2+2+2

-}

{-
sequence :: MOnad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
  where
    k :: Monad m => m a -> m [a] -> m [a]
    k m m' = do
      x <- m
      xs <- m'
      return (x:xs)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

-}

{-
*IO> sequence_ [Just 1, Just 2]
Just ()
*IO> sequence [Just 1, Just 2]
Just [1,2]
*IO> sequence [Just 1, Just 2, Nothing]
Nothing
*IO> sequence [getLine, getLine]
ab
cde
["ab","cde"]

*IO> mapM_ putChar "Hello"
Hello
*IO> mapM putChar "Hello"
Hello[(),(),(),(),()]
-}

{-
**
На этом шаге вы будете работать с монадой IO, а значит, ваша программа будет взаимодействовать с операционной системой. Чтобы тестирующая система смогла оценить вашу программу, пожалуйста, используйте только функции, работающие с файлами и директориями: getDirectoryContents, removeFile. Все эти функции уже будут находиться в области видимости, так что вам не следует их импортировать. По той же причине, главная функция вашей программы будет называться не main, а main' (со штрихом).

В этом задании ваша программа должна попросить пользователя ввести любую строку, а затем удалить все файлы в текущей директории, в именах которых содержится эта строка, выдавая при этом соответствующие сообщения.

Substring:

Пользователь вводит любую строку:

Substring: hell


Затем программа удаляет из текущей директории файлы с введенной подстрокой в названии. К примеру, если в текущей директории находились файлы thesis.txt, kitten.jpg, hello.world, linux_in_nutshell.pdf, то вывод будет таким:

Substring: hell
Removing file: hello.world
Removing file: linux_in_nutshell.pdf


Если же пользователь ничего не ввёл (просто нажал Enter), следует ничего не удалять и сообщить об этом:

Substring:
Canceled


Для получения списка файлов в текущей директории используйте функцию getDirectoryContents, передавая ей в качестве аргумента строку, состоящую из одной точки  ("."), что означает «текущая директория». Для удаления файлов используйте функцию removeFile (считайте, что в текущей директории нет поддиректорий — только простые файлы). В выводимых сообщениях удаленные файлы должны быть перечислены в том же порядке, в котором их возвращает функция getDirectoryContents.

Пожалуйста, строго соблюдайте приведенный в примере формат вывода. Особое внимание уделите пробелам и переводам строк! Не забудьте про пробел после Substring:, а также про перевод строки в конце (ожидается, что вы будете использовать putStrLn для вывода сообщений об удалении).

import System.Directory (getDirectoryContents, removeFile)
import Data.List (isInfixOf, filter)
import Control.Monad(liftM)

main' :: IO ()
main' = do
  putStr $ "Substring: "
  pattern <- getLine
  if null pattern
    then putStrLn "Canceled"
    else getFiles pattern >>= mapM_ deleteFile

getFiles :: String -> IO [FilePath]
getFiles pattern =
  liftM (filter (isInfixOf pattern)) $ getDirectoryContents "."

deleteFile :: FilePath -> IO ()
deleteFile path = do
  putStrLn $ "Removing file: " ++ path
  removeFile path
**
-}