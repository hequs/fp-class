import Control.Monad
import Data.List
import Data.Maybe
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

{- loadInventory -}
loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = readFile fname >>= return . (map readArmorItem . lines)
	where readArmorItem = (\(k : t : _) -> ArmorItem (read k) (read t)) . words


{- buildArmorKit -}
filterItems k list = list >>= (\x -> if checkArmorKind k x then [x] else [])
	where checkArmorKind k0 (ArmorItem k _) = k == k0

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit k list = if ((length $ nub fList) == (length fullSet)) then Just (ArmorKit k fullSet) else Nothing
	where
		fList = filterItems k list
		fullSet = [Shield ..]


{- buildKits -}
{-
getEachKindKits list = kinds >>= return . (\x -> buildArmorKit x list)
	where kinds = [Chitin ..]
	
getAllKits list = getEachKindKits list >>= (\x -> if isJust x then [fromJust x] else [])
-}

getAllKits list = kinds >>= return . (\x -> buildArmorKit x list) >>= (\x -> if isJust x then [fromJust x] else [])
	where kinds = [Chitin ..]

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits list = if (length kits > 0) then Just kits else Nothing
	where kits = getAllKits list

	
{- main -}	
main = (head `liftM` getArgs) >>= loadInventory >>= return . buildKits >>= print
