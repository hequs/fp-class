import Control.Monad
import Data.Maybe

{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))
  p >>= q = Parser (\s0 -> apply p s0 >>= (\(x, s1) -> apply (q x) s1))
  fail _ = Parser(\x -> Nothing)

instance MonadPlus Parser where
  mzero = Parser(\x -> Nothing)
  p `mplus` q = Parser (\s ->
	let ps = apply p s in
		if isNothing ps then apply q s else ps)

