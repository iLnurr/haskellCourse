module Demo where

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