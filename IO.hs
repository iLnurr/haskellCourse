module IO where

promptLine :: String -> IO String
promptLine prompt = do
  putStr "Name: "
  getLine

main = do
  putStrLn "What is your name?"
  name <- promptLine "Name: "
  putStrLn $ "Nice to meet you, " ++ name ++ "!"


{-

-}