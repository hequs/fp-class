{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type Factor = Int
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
integer' :: Parser Int
integer' = (*) <$> minus <*> (token natural)
  where
    minus = (char '-' >> return (-1)) <|> return 1
	
summand = Summand `liftM` ((,) <$> coef <*> power)
	where
		coef = integer' <|> return 1
		power = ((char 'x') >> power') <|> return 0
		power' = (char '^' >> integer) <|> return 1

p_sum (Poly p1) (Poly p2) = Poly (p1 ++ p2)

summand_to_ploy s = Poly [s]

{-
poly :: Parser Poly
poly = token (summand >>= rest addop summand)
	where
		rest op unit e1 = optional e1 $ do 
			e2 <- op >> unit
			rest op unit $ p_sum e1 e2
		addop = binop ("+", Plus) ("-", Minus)
		binop (s1, cons1) (s2, cons2) =
			(symbol s1 >> return cons1) <|>
			(symbol s2 >> return cons2)
-}

poly :: Parser Poly
poly = Poly `liftM` (token (many summand))

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
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
