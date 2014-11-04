import System.Random
import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS

{- getRandomList возвращает набор из 100 элементов, не содержащий 0 (!) -}
getRandomList :: IO [Int]
getRandomList = do
  gen <- newStdGen
  return $ take 100 $ randomRs(1, 100) gen 

{- добавляем в set несколько 0, в результате фактически должен добавиться только один -}
checkSet1 set = toList (foldl push set [0, 0, 0]) == toList (push set 0)

{- добавляем в set 0, затем его удаляем -}
checkSet2 set = toList (remove (push set 0) 0) == toList set

{- удаляем несуществующий элемент из set -}
checkSet3 set = toList (remove set 0) == toList set

main = do
	rList <- getRandomList	
	
	print $ checkSet1 (fromList rList :: LS.ListSet Int) 
	print $ checkSet1 (fromList rList :: TS.TreeSet Int)
		
	print $ checkSet2 (fromList rList :: LS.ListSet Int) 
	print $ checkSet2 (fromList rList :: TS.TreeSet Int)
	
	print $ checkSet3 (fromList rList :: LS.ListSet Int) 
	print $ checkSet3 (fromList rList :: TS.TreeSet Int)