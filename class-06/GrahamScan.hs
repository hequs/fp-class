{-# LANGUAGE EmptyDataDecls #-}
module GrahamScan where

import Data.Ord
import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point { x :: Double, y :: Double }
	deriving (Show, Eq)
	
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = L | S | R
	deriving (Show, Eq)
	
{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

directions :: [Point] -> [Direction]
directions (a:b:c:rest) = [findDirection a b c] ++ directions (b:c:rest)
directions _ = []

findDirection a b c 
	| res > 0 = L 
	| res < 0 = R
	| otherwise = S
		where
			res = ux * vy - uy * vx
			(ux, uy) = (x b - x a, y b - y a)
			(vx, vy) = (x c - x a, y c - y a)
		
{- directions [Point 1 2, Point 2 4, Point 4 5, Point 6 4, Point 7 2, Point 9 2] -> [R, R, R, L]-}

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

graham_scan :: [Point] -> [Point]
graham_scan list = foldl cleanStack stack restPoints
	where (stack, restPoints) = prepareData list

cleanStack (x1:x0:rest) x2
	| findDirection x0 x1 x2 /= L = cleanStack (x0:rest) x2
	| otherwise = (x2:x1:x0:rest)

prepareData list = ([head sList] ++ [p0], tail sList)
	where 
		sList = sortBy (compareAxisAngle p0) (tail ySortedList)
		p0 = head ySortedList
		ySortedList = sortBy comparePointY list

comparePointY a b
	| y a < y b = LT
	| otherwise = compare (x a) (x b)

compareAxisAngle a b1 b2 = compare (getAxisAngle a b1) (getAxisAngle a b2)
getAxisAngle a b = atan ((y b - y a) / (x b - x a))

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_scan_test1 = graham_scan [Point 4 4, Point 6 3, Point 2 2, Point 9 5, Point 2 6, Point 6 7, Point 7 2] 
	== [Point 2 6, Point 6 7, Point 9 5, Point 7 2, Point 2 2]

graham_scan_test2 = graham_scan [Point 4 4, Point 6 3, Point 2 2, Point 5 5, Point 2 6, Point 6 7, Point 7 2, Point 5 4, Point 4 5, Point 3 5] 
	== [Point 2 6, Point 6 7, Point 7 2, Point 2 2]


