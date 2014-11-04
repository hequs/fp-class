{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
	toList :: a -> [a]
	fromList :: [a] -> a
   
{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
	toList = words
	fromList = unwords

digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

undigits = foldl (\acc x -> acc * 10 + x) 0

instance Listable Integer where
	toList = digits
	fromList = undigits