module TreeSet (TreeSet, empty, push, contains, remove, toList, fromList) where

import AbstractSet

type TreeSet t = Tree t
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)


removeMax Empty = (Nothing, Empty)
removeMax (Node x l Empty) = (Just x, l)
removeMax (Node x l r) = (Just max, remove (Node x l r1) max)
	where
		(Just max, r1) = removeMax r
	
instance AbstractSet Tree where
  empty = Empty
  
  push Empty x = Node x Empty Empty
  push (Node x l r) y 
	| y < x = Node x (push l y) r
	| y > x = Node x l (push r y)
	| otherwise = Node x l r
  
  contains Empty _  = False
  contains (Node x l r) y 
	| y < x = contains l y
	| y > x = contains r y
	| otherwise = True

  remove Empty _  = Empty
  remove (Node x l r) y	
	| y < x = Node x (remove l y) r
	| y > x = Node x l (remove r y)
	| otherwise = case removeMax l of
      (Just z, t)  -> Node z t r
      (Nothing, _) -> r	
  
  toList Empty = []
  toList (Node x l r) = (toList r) ++ [x] ++ (toList l)
  
  fromList = foldl push empty