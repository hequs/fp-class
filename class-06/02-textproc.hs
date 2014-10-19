{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Environment
import System.Directory
import Data.List
import Data.Char
import System.IO
import System.Random

main = do
	(action : file : args) <- getArgs
	doAction action file args

doAction action file args
	| action == "1" = countRows file
	| action == "2a" = append file args
	| action == "2b" = prepend file args
	| action == "3" = toUpper' file
	| action == "4" = merge file args
	| action == "5" = genRandom file (map read args)
	
countRows file = do
	contents <- readFile file
	putStrLn $ show (length $ lines contents)

append file [str] = appendFile file ("\n" ++ str)

prepend file [str] = do
	contents <- readFile file
	let tempFile = ("_" ++ file)
	writeFile tempFile (str ++ "\n" ++ contents)
	renameFile tempFile file
	
toUpper' file = do
	contents <- readFile file
	let tempFile = ("_" ++ file)
	let contents' = map toUpper contents
	putStrLn contents'
	writeFile tempFile contents' 
	renameFile tempFile file
	
{- 
merge file1 [file2] = do
	c1 <- readFile file1
	c2 <- readFile file2
	let tempFile = ("_" ++ file1)
	writeFile tempFile $ unlines $ zipWith (++) (lines c1) (lines c2)
	renameFile tempFile file1
-}

merge file1 [file2] = do
	h1 <- openFile file1 ReadMode
	h2 <- openFile file2 ReadMode
	c1 <- hGetContents h1
	c2 <- hGetContents h2
	let tempFile = ("_" ++ file1)
	writeFile tempFile $ unlines $ zipWith (++) (lines c1) (lines c2)
	hClose h1
	hClose h2
	renameFile tempFile file1

genRandomStr len = do
	gen <- getStdGen
	return $ take len $ randomRs ('a','z') gen
	
genRandom file [l, s] = do
	if l > 0
	then do
		str <- genRandomStr s
		appendFile file (str ++ "\n")
		genRandom file [l - 1, s]
	else
		return ()