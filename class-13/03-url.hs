import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.List
import Data.Maybe

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Auth = (String, String)
type Server = String
type Port = String
type Path = String
type Params = String
type Anchor = String

data URL = URL Scheme Auth Server Port Path Params Anchor
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

get_to s = many1 (sat (\e -> isNothing $ elemIndex e s))

get_to_with s = do
	l <- get_to s
	getc
	return l
	
auth = (,) <$> get_to_with ":" <*> get_to_with "@"
port = (char ':' >> get_to "/?#")
path = (char '/' >> get_to "?#")
params = (char '?' >> get_to "#")
anchor = (char '#' >> many (sat $ const True))

parse_maybe p d = p <|> return d
	  
url = URL <$>
      scheme <*>
	  (string "://" >> (parse_maybe auth ("", ""))) <*>
      (get_to ":/?#") <*>	  
	  (parse_maybe port "") <*>
	  (parse_maybe path "") <*>
	  (parse_maybe params "") <*>
	  (parse_maybe anchor "")
	  
{- 
parse url "https://admin:123456@www.xxx.com:8080/login?page=1#2"
parse url "https://www.xxx.com:8080?page=1"
parse url "https://www.xxx.com"
-}