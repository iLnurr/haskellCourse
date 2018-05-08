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