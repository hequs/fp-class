{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (x : xs) = foldr (\x acc -> x >>= (intersectElem acc)) x xs
	where
		intersectElem list elem = filter (elem==) list