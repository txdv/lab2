module Main where

import Lib
import Control.Lens
import Network.Wreq
import Data.List
import Data.Char(ord)
import Parser(apply, JsonValue(JsonMap, JsonList, JsonString, JsonInt), jvalue, apply, getValue)
import System.Random.Shuffle
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B

{-
main :: IO ()
main = do
  r <- get "https://bentkus.eu"
  putStrLn $ show r
  putStrLn $ show $ r ^. responseBody
-}

data MoveResult = Miss | Hit
  deriving Show

data Coordinate = Coord (Int, Int) | CoordPrev (Int, Int) MoveResult Coordinate | GameOver | STRING String | EmptyCoord
  deriving Show

convertCoord (JsonList [JsonString a, JsonString b]) = Coord (convertCoord' a, convertCoord' b)
convertCoord (JsonMap [(JsonString "coord", b)]) = convertCoord b
convertCoord (JsonMap a) =
  let jsonMap = (JsonMap a)
  in convertCoord2 (convertCoord $ getJson "coord" jsonMap) (getJson "result" jsonMap) (getJson "prev" jsonMap)
convertCoord a = STRING $ show a
convertCoord2 (Coord (a, b)) result prev = CoordPrev (a, b) (moveResult result) (convertCoord prev)

moveResult (JsonString "HIT")  = Hit
moveResult (JsonString "MISS") = Miss

equal :: JsonValue -> JsonValue -> Bool
equal (JsonInt i) (JsonInt j) = i == j
equal (JsonString i) (JsonString j) = i == j
equal _ _ = False

getJsonMap :: JsonValue -> JsonValue -> Maybe JsonValue
getJsonMap searchKey (JsonMap ((key, value):acc)) =
  if key `equal` searchKey then Just value
  else getJsonMap searchKey (JsonMap acc)
getJsonMap searchKey _ = Nothing

getJson :: String -> JsonValue -> JsonValue
getJson searchKey map = getJson' searchKey map (getJsonMap (JsonString searchKey) map)
getJson' searchKey map (Just jsonValue) = jsonValue
getJson' searchKey map a = JsonString ((show a) ++ " " ++ searchKey ++ " " ++ (show map))

convertCoord' "A" = 0
convertCoord' "B" = 1
convertCoord' "C" = 2
convertCoord' "D" = 3
convertCoord' "E" = 4
convertCoord' "F" = 5
convertCoord' "G" = 6
convertCoord' "H" = 7
convertCoord' "I" = 8
convertCoord' "J" = 9
convertCoord' "1" = 0
convertCoord' "2" = 1
convertCoord' "3" = 2
convertCoord' "4" = 3
convertCoord' "5" = 4
convertCoord' "6" = 5
convertCoord' "7" = 6
convertCoord' "8" = 7
convertCoord' "9" = 8
convertCoord' "19" = 8


{-
putRow :: [String] -> Int -> String -> [String]
putRow row pos value = row & element pos .~ value

putTable :: [[String]] -> (Int, Int) -> String -> [[String]]
putTable table (a, b) value = table & element a .~ ["a"]
-}

--fillTable :: Coordinate -> [[String]] -> [[String]


data PlayerMove = Move (Int, Int) MoveResult
  deriving Show

getCoord (CoordPrev coord _ _) = coord
getCoord (Coord coord) = coord


--splitLists :: [a] -> [[a]]
--splitLists ls = splitLists' [[], []] ls
splitLists' [l, r] (x:y:xs) = splitLists' [l ++ x, r ++ y] xs
splitLists' [l, r] (x:xs)   = splitLists' [l ++ x, r     ] xs

getMoves :: Coordinate -> [PlayerMove]
getMoves (CoordPrev coord result prev) = [(Move (getCoord prev) result)] ++ getMoves prev
getMoves _ = []

putTable :: [[String]] -> (Int, Int) -> String -> [[String]]
putTable table (a, b) value = let l = table in l & ix a . ix b .~ value

emptyTable = map (\row -> map (\element -> "   ") row) table

--https://stackoverflow.com/questions/856845/how-to-best-way-to-draw-table-in-console-app-c

divider = "───" ++ (intercalate "───" $ map (\i -> "┼") [0..8]) ++ "───"
formatTable :: [[String]] -> String
formatTable table = intercalate ("\n" ++ divider ++ "\n") $ map (\row -> intercalate "│" row) table

table = do
  a <- [0..9]
  return $ [0..9]

showTable [r:rs] = "ASD"

main :: IO()
main = do
  --putStrLn $ show (filter (\e -> e /= "a") ["a", "b"])
  --putStrLn $ show table
  --i <- shuffleM table
  --putStrLn $ show $ i
  --putStrLn $ show $ filter (`notElem` [(0,0), (1,1)]) i
  [file] <- getArgs
  fileContents <- readFile file
  let res = getValue $ apply jvalue fileContents
  let coord = convertCoord res
  let moves = getMoves coord
  putStrLn $ show $ coord
  putStrLn $ show $ moves
  --putStrLn $ show $ splitLists' [[], []] moves
  --putStrLn $ show $ res
  putStrLn $ formatTable $ putTable emptyTable (0, 0) "---"
  --putStrLn $ show $ putRow ["a", "b"] 1 "c"
  --putStrLn (printTable emptyTable)
