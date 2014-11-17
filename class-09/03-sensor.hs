import System.Environment
import Data.Monoid
import Data.List.Split
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData content = map (\x -> if x == "-" then Nothing else (Just $ read x)) (lines content)

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay = chunksOf 5

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 True sData = minimum $ map (fromMaybe (minBound :: Int) . getFirst . mconcat . map First) sData
minData1 False sData = minimum $ map (fromMaybe (minBound :: Int) . getLast . mconcat . map Last) sData

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 True sData = minimum $ map (getSum . mconcat . map (Sum . fromJust) . filter isJust) sData
minData2 False sData = minimum $ map (getProduct . mconcat . map (Product . fromJust) . filter isJust) sData

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData NeedFirst = minData1 True
minData NeedLast = minData1 False
minData NeedSum = minData2 True
minData NeedProduct = minData2 False

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

countNoData sData = length $ filter (==True) $ map (getAll . mconcat . map (All . isNothing)) sData
countAllData sData = length $ filter (==True) $ map (getAll . mconcat . map (All . isJust)) sData
countAnyData sData = length $ filter (==True) $ map (getAny . mconcat . map (Any . isJust)) sData
countSumGreater sData n = length $ filter (>n) $ map (getSum . mconcat . map (Sum . fromJust) . filter isJust) sData
countProdGreater sData n = length $ filter (>n) $ map (getProduct . mconcat . map (Product . fromJust) . filter isJust) sData
countFirstGreater sData n = length $ filter (>n) $ map (fromMaybe (minBound :: Int) . getFirst . mconcat . map First) sData
countLastGreater sData n = length $ filter (>n) $ map (fromMaybe (minBound :: Int) . getLast . mconcat . map Last) sData

main = do
  fname <- head `fmap` getArgs
  sData <- fmap (dataByDay . getData) (readFile fname)
  putStrLn $ "NoDataCount: " ++ (show $ countNoData sData)
  putStrLn $ "AllDataCount: " ++ (show $ countAllData sData)
  putStrLn $ "AnyDataCount: " ++ (show $ countAnyData sData)
  putStrLn $ "SumGreaterCount: " ++ (show $ countSumGreater sData 6)
  putStrLn $ "ProdGreaterCount: " ++ (show $ countProdGreater sData 2)
  putStrLn $ "FirstGreaterCount: " ++ (show $ countFirstGreater sData 1)
  putStrLn $ "LastGreaterCount: " ++ (show $ countLastGreater sData 1000)