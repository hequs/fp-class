{-
1. �������� ���������, ������ ������� ����������� ���������������� ������, ���������� ������ ���������� �������:
��� ����=��������
���������� ������� ����� �������� summand (���������), multiplier (���������), divisor (��������). ��� ��������
�������� ������ �������. 

� �������� ���������� ��������� ������ ��������� �������� ��� ����������������� �����
� ��� ���������� ����� � �������������� �������.

��� ������ ����� ������ �� ������� ����� ����������� ��������,
��������� � ���������������� �����, �� ���� ����� ������������, ���������� � ������� ��������������.

���� �����-���� ���� �����������, �� �������� �� �����������. ���������� ���������� ��������� �� �������.
������������ ������ � ���������� ����������������� ����� ���������� ������ Reader.
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
--�� �����-�� ������� ������ "summand=" �������� �������� ����� validateOp
--� ���� �������� ��� ������ � validateOp', �� ghci ������
--��� � ghci ��� ��������� �������?

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
	
	
	
	