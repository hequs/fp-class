{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}

import Control.Monad
import System.Random
import Data.List

check s0 s = (cows - bulls, bulls)
	where
		cows = foldl (\count x -> if elemIndex x s0 /= Nothing then count + 1 else count) 0 s
		bulls = fst $ foldl (\(count, pos) x -> if elemIndex x s0 == Just pos then (count + 1, pos + 1) else (count, pos + 1)) (0, 0) s
		
genNumber gen0 = if nub (show num) == show num then num else genNumber gen
	where
		(num, gen) = randomR (1000,9999) gen0 :: (Int, StdGen)
	
	
playRound goal = do
	putStr "Print number: "
	number <- getLine
	let (cows, bulls) = check goal number
	if bulls == 4
		then
			putStrLn "Win!"
		else do
			putStrLn $ show (cows, bulls)	
			playRound goal
	
main = do
	putStrLn "Note: (Bulls, Cows)"
	gen <- newStdGen
	playRound $ show $ genNumber gen
