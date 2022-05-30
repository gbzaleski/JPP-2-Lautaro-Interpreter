
module TableModule where

import Data.Map (Map, fromList, insert, findWithDefault)

pmap :: (a -> b) -> [a] -> [b]
pmap = Prelude.map

updateLst :: Int -> a -> [a] -> [a]
updateLst _ _ [] = []
updateLst n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x : updateLst (n-1) newVal xs

data Table a = Table {
    defaultValue :: a,
    values :: Map [Int] a,
    len :: [Int]
  } deriving (Show, Eq)

getValue :: Table a -> [Int] -> a
getValue tb pos = findWithDefault (defaultValue tb) pos (values tb)

getDimensions :: Table a -> Int
getDimensions tb = length (len tb)

getLen :: Table a -> Int -> Maybe Int
getLen tb i = if 0 <= i && i < length (len tb) then Just $ len tb !! i else Nothing

updateTable :: Table a -> [Int] -> a -> Table a
updateTable tb pos newVal = tb {values = insert pos newVal $ values tb, len = zipWith max (pmap (+1) pos) $ len tb} 

createTable :: a -> [([Int], a)] -> Table a
createTable def initValues = Table {defaultValue = def, values = fromList initValues, len = initLen} where
    initLen = pmap (+1) $ foldr1 (zipWith max) $ pmap fst initValues

createTableInt :: [([Int], Int)] -> Table Int
createTableInt = createTable 0 

createTableString :: [([Int], String)] -> Table String
createTableString = createTable ""

createTableBool :: [([Int], Bool)] -> Table Bool
createTableBool = createTable False