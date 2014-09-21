-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms _s = (h, m, s)
	where
		h = div _s 3600
		m = div (mod _s 3600) 60
		s = mod _s 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- triangle :: ??? -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
  where
    p = distance (x1, y1) (x2, y2) + distance (x2, y2) (x3, y3) + distance (x3, y3) (x1, y1)
    s = abs (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = c + nEven xs
	where
		c
			| odd x = 0
			| otherwise = 1

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = (x * 2) : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| odd x = x : fltOdd xs
	| otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

dltNeg [] = []
dltNeg (x:xs)
	| x < 0 = dltNeg xs
	| otherwise = x : dltNeg xs

doubleEvenElems [] = []
doubleEvenElems (x:xs) 
	| even x = (x * 2) : doubleEvenElems xs
	| otherwise = x : doubleEvenElems xs

reversePlus [] = []
reversePlus (x1:x2:xs) = x2 : x1 : reversePlus xs
reversePlus (x) = []


-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x + y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

combine_plus_pair :: [Integer] -> [Integer] -> [(Integer, Integer)]
combine_plus_pair [] _ = []
combine_plus_pair _ [] = []
combine_plus_pair (x:xs) (y:ys) = (x, y) : combine_plus_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;

natDec n
	| n > 0 = n : natDec (n - 1)
	| otherwise = []
			
-- б) в порядке возрастания.

natInc n = natInc' 1
	where
		natInc' x
			| x <= n = x : natInc' (x + 1)
			| otherwise = []
			
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

insertElemPlus a (x1:x2:xs) = x1 : a : insertElemPlus a (x2:xs)
insertElemPlus _ x = x

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

divideListPlus (x:xs) = divideListPlus' ([x], xs)
	where 
		divideListPlus' (list, []) = (list, [])
		divideListPlus' ((x:xs), (y:ys))
			| y == x = divideListPlus' (x:x:xs, ys)
			| otherwise = (x:xs, y:ys)

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
--Возвращает i-й элемент списка
getElem (x:xs) n
	| n > 0 = getElem xs (n-1)
	| otherwise = x

-- б) Eq a => [a] -> a -> Bool
--Проверяет, если ли такой элемент в списке
listContains [] _ = False
listContains (x:xs) a
	| x == a = True
	| otherwise = listContains xs a
	
-- в) [a] -> Int -> [a]
--Возващает подмножество из первых n элементов списка
subList (x:xs) n
	| n > 0 = x : subList xs (n-1)
	| otherwise = []
	
-- г) a -> Int -> [a]
--Возвращает список из n повторяющихся элементов a
propagateElem a n
	| n > 0 = a : propagateElem a (n-1)
	| otherwise = []

-- д) [a] -> [a] -> [a]
--Объединяет списки
listUnion [] list = list
listUnion (x:xs) list = x : listUnion xs list

-- е) Eq a => [a] -> [[a]]
--Группирует элементы в списке
groupList [] = []
groupList l = [g] ++ groupList x
	where (g, x) = divideListPlus l

-- ж) [a] -> [(Int, a)]
--Нумерует список
numList list = zip' [1,2..] list
	where
		zip' _ [] = []
		zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- з) Eq a => [a] -> [a]
--Выбирает элменты, совпадающие с первым
cutAsFirst (x:xs) = cutAsFirst' x (x:xs)
	where
		cutAsFirst' x0 (x:xs)
			| x == x0 = x : cutAsFirst' x0 xs
			| otherwise = []
