import System.Random
import System.Environment
import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ

getRandomList :: Int -> IO [Int]
getRandomList n = do
  gen <- newStdGen
  return $ take n $ randomRs(1, 100) gen
  
  
enqueueN q n list = foldl enqueue q (take n list)

dequeueN q n = foldl (\acc _ -> snd $ dequeue acc) q [1..n]

testQueue' list q n = dequeueN (enqueueN q n list) (n-1)				
testQueue q list = foldl (testQueue' list) (enqueueN q 1 list) [2..length list]			

toList q = if (isEmpty q) then [] else (e : toList q')
	where
		(e, q') = dequeue q
		
main = do
	(arg0 : _) <- getArgs
	let n = read arg0
	list <- getRandomList n
	--putStrLn $ show list
	let l1 = toList $ enqueueN (empty :: Q.Queue Int) n list
	let l2 = toList $ enqueueN (empty :: FQ.Queue Int) n list
	let l3 = toList $ enqueueN (empty :: SQ.Queue Int) n list
	putStrLn $ show (list == l1 && l1 == l2 && l2 == l3)