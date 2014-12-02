{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import System.Random
import System.IO.Unsafe

data AppConfig = AppConfig {
		cfgMinDepth :: Int,
		cfgMaxDepth :: Int,
		cfgMinWidth :: Int,
		cfgMaxWidth :: Int,  
		cfgMinFileNum :: Int,
		cfgMaxFileNum :: Int,
		cfgFileSize :: Int
    } deriving (Show)

data AppState = AppState {
	  stCurFileNum :: Int,
	  stCurDepth :: Int,
      stCurPath :: FilePath
    } deriving (Show)

newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> [Int] -> FilePath -> IO a
runMyApp app [minDepth, maxDepth, minWidth, maxWidth, minFileNum, maxFileNum, fileSize] path =
    let config = AppConfig minDepth maxDepth minWidth maxWidth minFileNum maxFileNum fileSize
        state = AppState 0 0 path
    in evalStateT (runReaderT (runA app) config) state


{- random -}	
randomDirName :: IO String
randomDirName = do
	gen <- newStdGen
	return $ take 8 $ randomRs ('a','z') gen
		
randomFileName :: IO String
randomFileName = do
	name <- randomDirName
	return $ name ++ ".txt"

randomNumber :: Int -> Int -> IO Int
randomNumber lb ub = do
	gen <- newStdGen
	return $ fst $ randomR (lb, ub) gen

	
createFile :: Int -> String -> IO ()
createFile n fname = do
	gen <- newStdGen
	let content = take n $ randomRs ('a','z') gen
	writeFile fname content


{- treeCreator -}	
treeCreator :: MyApp ()
treeCreator = do
	minDepth <- cfgMinDepth `liftM` ask
	maxDepth <- cfgMaxDepth `liftM` ask
	goalDepth <- liftIO $ randomNumber minDepth maxDepth
	
	minWidth <- cfgMinWidth `liftM` ask
	maxWidth <- cfgMaxWidth `liftM` ask
	goalWidth <- liftIO $ randomNumber minWidth maxWidth
	
	minFileNum <- cfgMinFileNum `liftM` ask
	maxFileNum <- cfgMaxFileNum `liftM` ask
	goalFileNum <- liftIO $ randomNumber minFileNum maxFileNum
	
	fileSize <- cfgFileSize `liftM` ask
	
	st <- get
	let curFileNum = stCurFileNum st
	let curDepth = stCurDepth st
	let curPath = stCurPath st
	
	when (curDepth < goalDepth) $ do
		forM_ [1..goalFileNum] $ \_ -> do	
			fileName <- liftIO $ randomFileName
			let filePath = curPath </> fileName
			liftIO $ createFile fileSize filePath
			
		forM_ [1..goalWidth] $ \_ -> do
			dirName <- liftIO $ randomDirName
			let newPath = curPath </> dirName
			liftIO $ createDirectory newPath
			let newDepth = curDepth + 1
			put $ st {stCurDepth = newDepth, stCurPath = newPath, stCurFileNum = curFileNum + goalFileNum}
			treeCreator


{- main -}
main = do
  (path : params) <- getArgs		
  createDirectory path
  res <- runMyApp treeCreator (map read params) path
  print res
  
{-
	:main "/TEST_12" 3 5 1 5 1 5 8
	
	минимальная глубина 						- 3
	максимальная глубина						- 5
	минимальное кол. поддиректорий в папке 		- 1
	максимальное кол. поддиректорий в папке 	- 5
	минимальное кол. файлов в папке 			- 1
	максимальное кол. файлов в папке 			- 5
	размер файла			 					- 8
-}
