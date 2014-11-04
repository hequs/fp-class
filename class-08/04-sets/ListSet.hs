module ListSet (ListSet, empty, push, contains, remove, toList, fromList) where

import AbstractSet
import qualified Data.List as List

newtype ListSet t = ListSetImpl [t]

instance AbstractSet ListSet where
  empty = ListSetImpl []
  
  push (ListSetImpl xs) x = if elem x xs then (ListSetImpl xs) else (ListSetImpl (x:xs))
  
  contains (ListSetImpl xs) x = elem x xs
  
  remove (ListSetImpl xs) x = ListSetImpl (removeFromList xs x) where
	removeFromList [] _ = []
	removeFromList (x:xs) x0
	  | x == x0 = xs
	  | otherwise = (x : removeFromList xs x0)
  
  toList (ListSetImpl xs) = List.sort xs
  
  fromList = foldl push empty