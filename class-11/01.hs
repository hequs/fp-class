{-
1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. 

В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными.

Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.

Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}

import System.Environment
import Control.Monad.Reader
import Text.Regex.Posix
import Data.List.Split
import Data.Maybe


{- doOp -}
doOp :: (String, Integer) -> Integer -> Integer
doOp ("summand", x) 	= (+ x)
doOp ("multiplier", x) 	= (* x)
doOp ("divisor", x) 	= (`div` x)


{- getReader -}
getReader :: [(String, Integer)] -> Reader Integer Integer
getReader ops = do
	x <- ask
	return $ foldl (\acc op -> (doOp op) acc) x ops


{- parseOp -}
validateOp str = str =~ "(summand|multiplier|divisor)=(-?[0-9]{1,})" :: Bool
validateOp' str = str =~ "(summand|multiplier|divisor)=(-?[0-9]{1,})" :: String
--по какой-то причине строка "summand=" проходит проверку через validateOp
--а если засунуть эту строку в validateOp', то ghci упадет
--баг в ghci или нерадивый студент?

parseOp' str = (head pStr, read (last pStr))
	where
		pStr = splitOn "=" str

parseOp :: String -> Maybe (String, Integer)
parseOp str = if (validateOp str) then Just(parseOp' str) else Nothing


{- main -}
main = do
	[configFile, numbersFile] <- getArgs
	config <- (readFile configFile >>= return . (map fromJust) . (filter isJust) . (map parseOp) . lines)
	let reader = getReader config
	numbers <- (readFile numbersFile >>= return . (map (\x -> read x :: Integer)) . lines)
	return $ map (runReader reader) numbers
	
	
	
	