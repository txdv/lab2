module Main where

import Lib
import Control.Lens
import Network.Wreq
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

table = do
  a <- [0..9]
  b <- [0..9]
  return $ (a, b)

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
  --putStrLn $ show $ res
  putStrLn $ show $ convertCoord res
