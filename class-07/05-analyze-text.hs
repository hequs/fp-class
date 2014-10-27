{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

import Data.Char
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import System.Environment

incKey key map = case (Map.lookup key map) of
	Just x 	-> Map.update (\value -> Just (value + 1)) key 	map
	Nothing -> Map.insert key 0 map
 	
task1 content = Map.keys $ Map.filter (== max) map0
    where 
		map0 = foldl' (\acc x -> if (isPunctuation x) then (incKey x acc) else acc) Map.empty content
		max = maximum $ Map.elems map0

task2 content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
    where
		list = filter (not . null) $ map (map toLower . filter isLetter) $ words content
		map0 = foldl' (\acc word -> incKey word acc) Map.empty list

task3_1 content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
    where 
		map0 = foldl' (\acc x -> if (x == ' ') then acc else incKey x acc) Map.empty content
		
task3_2 content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
    where 
		map0 = fst $ foldl' (\(acc, prev) x -> (if (x == ' ' || prev == ' ') then acc else incKey [prev:x:[]] acc, x)) (Map.empty, ' ') content		
				
task3_3 content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
    where 
		list = filter (\x -> isLetter x || x == ' ') content
		(map0, _, _) = foldl' (\(acc, p1, p0) x -> (if (x == ' ' || p0 == ' ' || p1 == ' ') then acc else incKey [p1:p0:x:[]] acc, p0, x)) (Map.empty, ' ', ' ') list	
	
main = do
	(fname : _) <- getArgs
	content <- readFile fname
	putStrLn ("Task1: " ++ (show $ task1 content))
	putStrLn ("Task2: " ++ (show $ task2 content))
	putStrLn ("Task3 (symbols): " ++ (show $ task3_1 content))
	putStrLn ("Task3 (digram): " ++ (show $ task3_2 content))
	putStrLn ("Task3 (trigram): " ++ (show $ task3_3 content))