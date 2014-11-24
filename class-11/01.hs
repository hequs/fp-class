{-
1. Ќаписать программу, работа которой управл€етс€ конфигурационным файлом, содержащим строки следующего формата:
им€ пол€=значение
¬озможными именами полей €вл€ютс€ summand (слагаемое), multiplier (множитель), divisor (делитель). ¬се значени€
€вл€ютс€ целыми числами. 

¬ качестве параметров командной строки программе подаютс€ им€ конфигурационного файла
и им€ текстового файла с целочисленными данными.

Ќад каждым целым числом из второго файла выполн€ютс€ операции,
указанные в конфигурационном файле, то есть число складываетс€, умножаетс€ и делитс€ соответственно.

≈сли какое-либо поле отсутствует, то действие не выполн€етс€. –езультаты вычислений вывод€тс€ на консоль.
ќрганизовать доступ к параметрам конфигурационного файла средствами монады Reader.
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
	
	
	
	