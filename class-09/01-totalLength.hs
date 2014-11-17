import System.Environment
import Data.Functor

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength list = sum (map length list)

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 _ 0 = Nothing
build1 c n = fmap (map (\x -> replicate x c)) (Just [1..n])

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 _ 0 = Left "n = 0"
build2 'x' _ = Left "Rospotrebnadzor zapreshhaet sozdavat' stroki iz simvola 'x'"
build2 c n
	| n > 100 = Left "n > 100"
	| otherwise = fmap (map (\x -> replicate x c)) (Right [1..n])

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

main = do
	(fname : c : n : []) <- getArgs
	t1 <- fmap totalLength getArgs
	putStrLn $ "Task1: " ++ show t1

	t2 <- fmap (totalLength . words) (readFile fname)
	putStrLn $ "Task2: " ++ show t2
	
	putStrLn $ "Task3_build1: " ++ show (fmap totalLength $ build1 (head c) (read n))
	putStrLn $ "Task3_build2: " ++ show (fmap totalLength $ build2 (head c) (read n))