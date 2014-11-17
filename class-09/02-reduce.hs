import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	| mod a 3 == 0 	= 0
	| odd a 		= a * a
	| otherwise 	= a * a * a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n f = foldl (\acc _ -> fmap reduce acc) f [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = fmap (\(x, y) -> x + y)

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe l = fmap (sum . toList) (Just l)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty"
toEither l = fmap (sum . toList) (Right l)

-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO l = do
	gen <- newStdGen
	return $ (sum $ toList l) * (fst $ randomR (-1, 1) gen)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs (fname : n : []) = (fname, read n)

convert (s1 : s2 : _) = (read s1 :: Int, read s2 :: Int)	
readData :: FilePath -> IO [(Int, Int)]
readData fname = do
	content <- readFile fname
	return $ map (convert . words) (lines content)

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}

{-
*Main> :main "02_1.txt" 1
[0,49]
Just 1000
Right 1000
1000

*Main> :main "02_2.txt" 2
[2401]
Just 2401
Right 2401
2401
-}