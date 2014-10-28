{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Random
import System.Environment
import Data.List
import Control.Monad

genRow cols bounds = do
	gen <- newStdGen
	return $ unwords $ map show $ take cols (randomRs bounds gen :: [Int])

genText rows cols bounds = do
	text <- liftM unlines $ replicateM rows $ genRow cols bounds
	return text
	
main = do
	[fname, min, max, cols, rows] <- getArgs	
	text <- genText (read rows :: Int) (read cols :: Int) (read min :: Int, read max :: Int)
	writeFile fname text