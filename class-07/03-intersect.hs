{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile fname = do
	content <- readFile fname
	return $ concatMap (map (\x -> read x :: Int) . words) $ lines content

solve lists = (length list, list)
	where
		list = Set.toList $ foldl1 Set.intersection $ map Set.fromList lists

main = getArgs >>= mapM readNumFile >>= print.solve
