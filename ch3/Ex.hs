data List a = Nil | Cons a (List a) deriving Show

fromList :: [a] -> List a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : (toList xs)

data ATree a = ATree a (Maybe (ATree a)) (Maybe (ATree a)) deriving Show

length' :: [a] -> Int
length' []       = 0
length' (x : xs) = 1 + length' xs
