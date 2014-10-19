{-
   Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
   на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.

   Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
   (для ghc и ghci), например:

     $ ghc 05-graham.hs  -o graham -i../class-05/3-Graham_scan/
-}

import GrahamScan
import System.Environment

toTuple (Point x y) = (x, y)

toPoint (x, y) = (Point x y)

main = do
	[file] <- getArgs
	contents <- readFile file
	let res = graham_scan $ map (toPoint . read) (lines contents)
	writeFile "graham_res.txt" (unlines $ map (show . toTuple) res)