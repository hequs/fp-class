{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import FloatParser
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

sign :: Parser Float
sign = (char '-' >> return (-1.0)) <|> (char '+' >> return 1.0)

factor :: Parser Float
factor = float <|> ((*) <$> sign <*> token (optional 1 float))

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

s_factor:: Summand -> Factor
s_factor (Summand (f, _)) = f

s_power :: Summand -> Power
s_power (Summand (_, p)) = p

poly_factor :: [Summand] -> Factor	--коэфициент при старшей степени
poly_factor (s : _) = s_factor s

poly_power :: [Summand] -> Power		--старшая степень
poly_power (s : _) = s_power s

poly_fill :: [Summand] -> [Summand]
poly_fill l = if s_power (last l) == 0 then (poly_fill' l) else (poly_fill' (l ++ [Summand (0,0)]))
	where
		poly_fill' (s : []) = [s]
		poly_fill' (s : rest) = if (next_sp == poly_power rest) then (s : poly_fill' rest) else (s : poly_fill' (Summand (0, next_sp) : rest))
			where next_sp = s_power s - 1

poly_mul :: [Summand] -> Summand -> [Summand]
poly_mul l (Summand (f, p)) = map (\(Summand (f0, p0)) -> (Summand (f0 * f, p0 + p))) l

poly_sub :: [Summand] -> [Summand] -> [Summand]
poly_sub l1 l2 = zipWith (\(Summand (f1, p1)) (Summand (f2, p2)) -> (Summand (f1-f2, p1))) (poly_fill l1) (poly_fill l2)

divmod' ch dm dt = if (poly_power dt > poly_power dm) then (ch, dm) else divmod' (ch ++ [diff]) (poly_sub dm (poly_mul dt diff)) dt
	where
		diff = Summand ((poly_factor dm) / (poly_factor dt), (poly_power dm) - (poly_power dt))

divmod :: Poly -> Poly -> (Poly, Poly)
divmod (Poly dm) (Poly dt) = (Poly p1, Poly p2)
	where (p1, p2) = divmod' [] dm dt

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}

poly_gcd :: Poly -> Poly -> Poly
poly_gcd p1 p2 = poly_gcd' p1 p2
	where
		poly_gcd' p1 (Poly []) = p1
		poly_gcd' p1 p2 = poly_gcd' p2 (snd $ divmod p1 p2)
  
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
