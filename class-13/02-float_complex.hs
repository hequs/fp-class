import Parser
import SimpleParsers
import ParseNumbers
import Data.Char

import Control.Applicative hiding (many, optional)
import Control.Monad

{- Напишите парсер для вещественных чисел. -}
digitToFloat :: Int -> Float
digitToFloat x = fromIntegral x :: Float

{-
leftPart = (+) <$> (digitToFloat `liftM` integer) <*> rest
rest = (char '.' >> rightPart)
rightPart = ((foldr (\n m -> (m + n) / 10) 0) . (map digitToFloat)) `liftM` many digit
-}

float :: Parser Float
float = (*) <$> minus <*> float'
	where
		minus = (char '-' >> return (-1)) <|> return 1
		float' = (+) <$> (digitToFloat `liftM` integer) <*> rightPart
		rightPart = char '.' >> (foldr (\n m -> (m + (digitToFloat n)) / 10) 0) `liftM` many digit


{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = undefined

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = undefined

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = undefined

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


