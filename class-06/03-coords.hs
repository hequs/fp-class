{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.Environment

data Point = Point { x :: Double, y :: Double } deriving (Eq)

main = do
	[action, file] <- getArgs
	case action of 
		"a" -> genPointFile file
		"b" -> countQuarters file
		"c" -> getTheFarthestPoint file

		
genPointFile file = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	let bound = 100
	let n = 10
	writeFile file $ unlines $ map show $ take n $ zipWith (\x y -> (x, y)) (randomRs (0, bound) gen1 :: [Int]) (randomRs (0, bound) gen2 :: [Int])
	
	
pointQuarterScan (q1, q2, q3, q4) (x, y)
	| x > 0 && y >= 0 = (q1 + 1, q2, q3, q4)
	| x <= 0 && y > 0 = (q1, q2 + 1, q3, q4)
	| x < 0 && y <= 0 = (q1, q2, q3 + 1, q4)
	| otherwise = (q1, q2, q3, q4 + 1)
	
countQuarters file = do
	contents <- readFile file
	let count = foldl (\acc x -> pointQuarterScan acc (read x)) (0, 0, 0, 0) (lines contents)
	putStrLn $ show count


countDist (x, y) = (x^2) + (y^2)

getTheFarthestPoint' p0 p1 = if (countDist p0 > countDist p1) then p0 else p1
	
getTheFarthestPoint file = do
	contents <- readFile file
	let point = foldl (\acc x -> getTheFarthestPoint' acc (read x)) (0, 0) (lines contents)
	putStrLn $ show point
	
	
	
	
	