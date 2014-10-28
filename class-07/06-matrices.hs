{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}

import System.Environment
import Data.Array.IArray
import qualified Data.Array as Array
import Data.List
import Data.List.Split

readMatrix fname = do
	content <- readFile fname
	let rows = lines content
	return $ listArray ((1, 1), (length rows, length rows)) $ (map (\x -> read x :: Int) $ concat $ map words rows)

writeMatrix fname m = do
	let ((_, _), (size, _)) = bounds m
	writeFile fname $ unlines $ map (unwords . map show) $ chunksOf size $ Array.elems m
		
sumMatrix :: Array (Int, Int) Int -> Array (Int, Int) Int -> IO (Array (Int, Int) Int)
sumMatrix x y = do
	return $ array resultBounds [((r, c), x!(r, c) + y!(r, c)) | r <- range(lr, ur), c <- range(lc, uc)]
		where
			bx@((lr, lc), (ur, uc)) = bounds x
			resultBounds
				| bx == bounds y = bx
				| otherwise = error "sumMatrix: incompatible bounds"


multMatrix :: Array (Int, Int) Int -> Array (Int, Int) Int -> IO (Array (Int, Int) Int)
multMatrix x y = do
	return $ array resultBounds [((i, j), sum [x!(i, k) * y!(k, j) | k <- range (lj,uj)]) | i <- range (li,ui), j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "multMatrix: incompatible bounds"
				
main = do
	m1 <- readMatrix "06_1.txt"
	m2 <- readMatrix "06_2.txt"
	m_sum <- sumMatrix m1 m2
	writeMatrix "06_sum.txt" m_sum
	m_mult <- multMatrix m1 m2
	writeMatrix "06_mult.txt" m_mult
	