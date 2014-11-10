import Control.Applicative

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 a1 a2 = max <$> a1 <*> a2

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 a1 a2 a3 = maxApp2 a1 (maxApp2 a2 a3)

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp = foldl1 maxApp2

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main = do
	print $ maxApp2 (Just 2) (Just 3)
	print $ maxApp3 (Just 1) (Just 2) (Just 3)
	print $ maxApp [Nothing, Just 1, Just 2, Just 3]
	print $ maxApp [Left "Left" :: Either String Int, Right 1 :: Either String Int]

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
