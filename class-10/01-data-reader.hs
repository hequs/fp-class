{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import Data.Ord
import Data.List

data Student = Student { name :: String, age :: Int, group :: String }

instance Show Student where
	show (Student n a g) = n ++ "\n" ++ (show a) ++ "\n" ++ g

readStudents [] = []
readStudents (name : age : group : list) = [Student name (read age) group] ++ (readStudents list)

writeStudents [] = []
writeStudents (s : list) = [show s] ++ (writeStudents list)
	
readSFile fname = (readStudents . lines) `liftM` (readFile fname)
writeSFile fname list = writeFile fname (unlines $ writeStudents list)

main = ((++) `liftM` (readSFile "01_s1.txt") `ap` (readSFile "01_s2.txt")) >>= (writeSFile "01_s.txt" . sortBy (comparing name))
