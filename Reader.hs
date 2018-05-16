module Reader where

{-
instance Functor ((->) e) where
  fmap g h = g . h

fmap :: (a -> b) -> f a -> f b
     :: (a -> b) -> (e -> a) -> (e -> b)

Prelude> :t fmap (^2) length
fmap (^2) length :: Foldable t => t a -> Int
Prelude> :t fmap (^2) length [1,2,3]
fmap (^2) length [1,2,3] :: Int
Prelude> fmap (^2) length [1,2,3]
9

instance Monad ((->) e) where
  -- return :: a -> e -> a
  return x = \_ -> x
  -- (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
  (>>=) m k = \e -> k (m e) e

-}

safeHead = do
  b <- null -- null :: [] -> Bool
  if b
  then return Nothing
  else do
    h <- head
    return $ Just h


{-
*Reader> safeHead []
Nothing
*Reader> safeHead [1,2]
Just 1
-}

safeHead' = do
  e <- id
  if null e
  then return Nothing
  else return $ Just (head e)
{-
newtype Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \e -> x
  (>>=) m k = Reader $ \e ->
    let v = runReader m e
    in runReader (k v) e

ask :: Reader r r
ask = Reader id

type User = String
type Password = String
type UsersTable = [(User,Password)]

pwds :: UsersTable
pwds = [("Bil","123"),("Ann","qwerty"),("John","sD3Fre")]

firstUser :: Reader UsersTable User
firstUser = do
  e <- ask
  return $ fst (head e)
-}