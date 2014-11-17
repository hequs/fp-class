import Control.Monad 
import Data.List

{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

unbalanced (l, r) = abs (l - r) > balance

updatePole :: Pole -> Either String Pole
updatePole (l, r)
	| unbalanced (l, r) && l > r = Left "Too many on left"
	| unbalanced (l, r) && l <= r = Left "Too many on right"
	| otherwise = Right (l, r)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

banana :: Pole -> Either String Pole
banana = const (Left "Banana")

{- 3 -}
landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = Right (left + n, right + n)

{- 4 -}
unlandAll = const (Right (0, 0))

{- 1 -}
getParametr row = read (drop 2 row) :: Int

readAction "B" = banana
readAction "U" = unlandAll
readAction row
	| (head row) == 'M' = landBoth $ getParametr row
	| (head row) == 'L' = landLeft $ getParametr row
	| (head row) == 'R' = landRight $ getParametr row

readActions [] = []
readActions (row : list) = [readAction row] ++ readActions list

readAFile = (readActions . lines) `liftM` (readFile "02.txt")

doActions actions = foldr (<=<) return actions (0, 0)

launchRopewalker = readAFile >>= return . doActions

{- 5 -}
test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "Too many on right"
test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Banana"
test 4 = (return (0, 0) >>= landLeft 1 >>= unlandAll) == Right (0, 0)
test 5 = (return (0, 0) >>= landBoth 1) == Right (1, 1)
	
tests = all test [1..5]