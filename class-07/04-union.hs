{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile fname = do
	content <- readFile fname
	return $ concatMap (map (\x -> read x :: Int) . words) $ lines content

solve lists = (length list, foldl1 (+) list)
	where
		list = Set.toList $ foldl1 Set.union $ map Set.fromList lists

main = getArgs >>= mapM readNumFile >>= print.solve