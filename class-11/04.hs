{-
4. ѕользу€сь средствами монады ST, запрограммировать сортировку массива трем€ любыми методами.
-}

import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray


swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
	vi <- readArray arr i
	vj <- readArray arr j
	writeArray arr i vj
	writeArray arr j vi

{- http://habrahabr.ru/post/204600/ -}
bubbleSort :: (Ord e) => [e] -> [e]
bubbleSort arr0 = elems $ runSTArray $ do	
	let l = length arr0
	arr <- newListArray (0, l - 1) arr0
	forM_ [0..(l - 2)] $ \i -> 
		forM_ [0..(l - i - 2)] $ \j -> do
			vj0 <- readArray arr j
			vj1 <- readArray arr (j + 1)
			when (vj0 > vj1) (swapElems j (j + 1) arr)
	return arr

{- http://habrahabr.ru/post/204600/ -}
shakerSort :: (Ord e) => [e] -> [e]
shakerSort arr0 = elems $ runSTArray $ do
	let l = length arr0
	arr <- newListArray (0, l - 1) arr0
	forM_ [0..(l `div` 2)] $ \i -> do
		forM_ [i..(l - i - 2)] $ \j -> do
			vj0 <- readArray arr j
			vj1 <- readArray arr (j + 1)
			when (vj0 > vj1) (swapElems j (j + 1) arr)
		forM_ [(l - i - 3)..i] $ \j -> do
			vj0 <- readArray arr j
			vj1 <- readArray arr (j + 1)
			when (vj0 > vj1) (swapElems j (j + 1) arr)
	return arr