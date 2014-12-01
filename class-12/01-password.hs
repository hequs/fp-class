{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import System.Environment
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Char

{- Config -}
type Config = (Int, [Bool])

parseConfig :: [String] -> Config
parseConfig (len : params) = (read len :: Int, map (\x -> read x :: Bool) params)


{- isValid -}
isValid :: String -> Reader Config Bool
isValid s = do
	(len, [a, n, p]) <- ask
	return $ length s >= len 
                && if a then any isAlpha s else True
                && if n then any isNumber s else True
                && if p then any isPunctuation s else True


{- getValidPassword -}
getValidPassword :: MaybeT (ReaderT Config (WriterT [String] IO)) String
getValidPassword = do
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  config <- lift ask
  guard (runReader (isValid s) config)
  return s


{- askPassword -}
askPassword :: MaybeT (ReaderT Config (WriterT [String] IO)) ()
askPassword = do	
	value <- msum $ repeat getValidPassword
	liftIO $ putStrLn "Сохранение в базе данных..."

	
{- main -}	
main = do
	config <- getArgs
	result <- runWriterT (runReaderT (runMaybeT askPassword) (parseConfig config))
	return $ result
	
{-
	:main "4" "False" "True" "True"
	
	минимальная длина 			- 4
	наличие букв				- False
    наличие цифр				- True
	наличие знаков пунктуации 	- True
-}
