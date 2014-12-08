{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Ord
import Data.List

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type Factor = Float
type Power = Int
data Summand = Summand (Factor, Power)
	deriving Show
data Op = Plus | Minus | Mul | Div
	deriving Show
data Poly = Poly [Summand]
	deriving Show
	
{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

sign :: Parser Int
sign = (char '-' >> return (-1)) <|> (char '+' >> return 1)

factor :: Parser Float
factor = integer <|> ((*) <$> sign <*> token (optional 1 float))

power :: Parser Int
power = char 'x' >> (optional 1 (char '^' >> integer))

summand = Summand `liftM` token (with_factor <|> without_factor)
	where
		with_factor = (,) <$> factor <*> optional 0 power
		without_factor = (,) <$> return 1 <*> power
		
poly :: Parser Poly
poly = Poly `liftM` (many summand)

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}

s_power :: Summand -> Factor
s_power (Summand (_, p)) = p

s_factor:: Summand -> Factor
s_factor (Summand (f, _)) = f

poly_power :: Poly -> Int	--старшая степень
poly_power (Poly (s : _)) = s_power s

poly_factor :: Poly -> Int	--коэфициент при старшей степени
poly_factor (Poly (s : _)) = s_factor s

poly_power_compare :: Poly -> Poly -> Ordering
poly_power_compare p1 p2 = compare (poly_power p1) (poly_power p2)

poly_fill :: Poly -> Poly
poly_fill (Poly l) = if s_power (last l) == 0 then Poly (poly_fill' l) else Poly (poly_fill' (l ++ [Summand (0,0)]))
	where
		poly_fill' (s : []) = [s]
		poly_fill' (s : rest) = if (next_sp == poly_power (Poly rest)) then (s : poly_fill' rest) else (s : poly_fill' (Summand (0, next_sp) : rest))
			where next_sp = s_power s - 1

poly_mul :: Poly -> Summand -> Poly
poly_mul (Poly l) (Summand (f, p)) = Poly (map (\(Summand (f0, p0)) -> (Summand (f0 * f, p0 + p))) l)
		
divmod :: Poly -> Poly -> (Poly, Poly)
divmod = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = undefined

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = undefined
