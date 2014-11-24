{-
2. ќрганизовать вычисление значений функций sin и cos, пользу¤сь р¤дами “ейлора и сохран¤¤ каждое слагаемое
в журнал посредством монады Writer. ¬ тексте программы допускаетс¤ только один вызов функции tell.
-}

import Control.Monad.Writer

eps = 0.000001

taylor :: (Double, Double) -> Double -> Double -> Writer [Double] Double
taylor (value, s) x n = tell [value] >> 
		if (abs(s + n_s) < eps) then return value
		else taylor (value + n_s, n_s) x (n + 2)
	where
		n_s = -1 * s * x * x / ((n + 1) * (n + 2))
		
{- в радианах -}
sin' x = runWriter $ taylor (x, x) x 1
cos' x = runWriter $ taylor (1, 1) x 0