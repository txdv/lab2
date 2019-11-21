module Main where

import Lib
import Control.Lens
import Data.List
import Data.Char(ord)
import Parser(apply, JsonValue(JsonMap, JsonList, JsonString, JsonInt), jvalue, apply, getValue)
import System.Random.Shuffle
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import RemoteGame

data MoveResult = Miss | Hit
  deriving Show

data Coordinate = Coord (Int, Int) | CoordPrev (Int, Int) MoveResult Coordinate | GameOver Coordinate | STRING String | EmptyCoord
  deriving Show

resultToJson Miss = "MISS"
resultToJson Hit = "HIT"

toJson (x, y) = "[\"" ++ xJson x ++ "\",\"" ++ yJson y ++ "\"]"
toJsonCoord (Coord coord) = "{\"coord\":" ++ toJson coord ++ "}"
toJsonCoord (GameOver prev) = "{\"coord\":[],\"prev\":" ++ toJsonCoord prev ++ "}"
toJsonCoord (CoordPrev coord result prev) = "{\"coord\":" ++ toJson coord ++ ",\"result\":\"" ++ resultToJson result ++ "\",\"prev\":" ++ toJsonCoord prev ++ "}"

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

xJson 0 = "A"
xJson 1 = "B"
xJson 2 = "C"
xJson 3 = "D"
xJson 4 = "E"
xJson 5 = "F"
xJson 6 = "G"
xJson 7 = "H"
xJson 8 = "I"
xJson 9 = "J"

yJson 0 = "1"
yJson 1 = "2"
yJson 2 = "3"
yJson 3 = "4"
yJson 4 = "5"
yJson 5 = "6"
yJson 6 = "7"
yJson 7 = "8"
yJson 8 = "9"
yJson 9 = "10"

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
convertCoord' "10" = 9

data PlayerMove = Move (Int, Int) MoveResult
  deriving Show

getCoord (CoordPrev coord _ _) = coord
getCoord (Coord coord) = coord

-- splits lists into two lists [x1, x2, x3, x4, x5, x6, ...] -> [[x1, x3, x5, ...], [x2, x4, x6, ...]]
splitLists :: [a] -> [[a]]
splitLists ls = splitLists' [[], []] ls
splitLists' :: [[a]] -> [a] -> [[a]]
splitLists' [l, r] (x:y:xs) = splitLists' [l ++ [x], r ++ [y]] xs
splitLists' [l, r] (x:xs)   = splitLists' [l ++ [x], r      ] xs
splitLists' acc _ = acc

getMoves :: Coordinate -> [PlayerMove]
getMoves (CoordPrev coord result prev) = [(Move (getCoord prev) result)] ++ getMoves prev
getMoves _ = []

putTable :: [[String]] -> (Int, Int) -> String -> [[String]]
putTable table (a, b) value = let l = table in l & ix b . ix a .~ value

emptyTable = map (\row -> map (\element -> "   ") row) table

putMoveTable table (Move coord Hit) = putTable table coord " X "
putMoveTable table (Move coord Miss) = putTable table coord " - "

putMovesTable table (move:moves) = putMovesTable (putMoveTable table move) moves
putMovesTable table _ = table

--https://stackoverflow.com/questions/856845/how-to-best-way-to-draw-table-in-console-app-c

divider = "───" ++ (intercalate "───" $ map (\i -> "┼") [0..8]) ++ "───"
formatTable :: [[String]] -> String
formatTable table = intercalate ("\n" ++ divider ++ "\n") $ map (\row -> intercalate "│" row) table

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

surounding coord = [add coord (0, 1), add coord (1, 0), add coord (0, -1), add coord (-1, 0)]

isValidCoord (x, y) = x >= 0 && y >= 0 && x < 10 && y < 10

moveCoord (Move coord _) = coord

nextMove moves = nextMove' moves moves []
nextMove' allMoves ((Move coord Hit):moves) nextMoves = nextMove' allMoves moves (nextMoves ++ (surounding coord))
nextMove' allMoves ((Move _    Miss):moves) nextMoves = nextMove' allMoves moves nextMoves
--nextMove' allMoves [] nextMoves = filter isValidCoord nextMoves
nextMove' allMoves [] nextMoves =
  let existingCoord = map moveCoord allMoves
  in filter isValidCoord $ filter (`notElem` existingCoord) nextMoves

allCoordinatesMax max = do
  x <- [0..max]
  y <- [0..max]
  return $ (x, y)

allCoordinates = allCoordinatesMax 9

randomCoordinates :: IO [(Int, Int)]
randomCoordinates = shuffleM allCoordinates

type PlayerState = [PlayerMove]

randomNextMove :: [PlayerMove] -> IO [(Int, Int)]
randomNextMove moves = do
  i <- randomCoordinates
  let moveCoords = map moveCoord moves
  let filtered = filter (`notElem` moveCoords) i
  return $ filtered

kitas :: PlayerState -> IO [(Int, Int)]
kitas moves = do
  nextMovesRandom <- randomNextMove moves
  -- make it more deterministic without shuffleM
  --let nextMovesDeterministic = nextMove moves
  nextMovesDeterministic <- shuffleM $ nextMove moves
  putStrLn $ ("HERE: " ++ (show moves))
  putStrLn $ show nextMovesDeterministic
  return $ nub (nextMovesDeterministic ++ nextMovesRandom)

table = do
  a <- [0..9]
  return $ [0..9]

shipShapes :: [[(Int, Int)]]
shipShapes = [
  [(0, 0), (1, 0), (2, 0), (3, 0)],
  [(0, 0), (1, 0), (1, 1), (2, 1)],
  [(0, 0), (1, 0), (0, 1), (1, 1)],
  [(1, 0), (0, 1), (1, 1), (2, 1)],
  [(0, 0), (1, 0), (2, 0), (2, 1)]]

shipSize :: [(Int, Int)] -> (Int, Int)
shipSize coords =
  let xs = map fst coords
      ys = map snd coords
  in (foldl (max) 0 xs + 1, foldl (max) 0 ys + 1)

shipFieldsLength = length $ concat shipShapes

-- generate probably invalid field
randomShipField :: IO [(Int, Int)]
randomShipField = do
  randomCoordinates <- shuffleM (allCoordinatesMax 8)
  let starting = take 5 randomCoordinates
      pos = zipWith (startShip) starting shipShapes
  return $ nub $ concat pos

-- just generate probably invalid fields, check for valids one and print them out
randomField :: IO [(Int, Int)]
randomField = do
  let attempts = [1 .. 10000]
      t = map (\i -> randomShipField) attempts
      listM = sequence t
  fields <- listM
  return $ head $ filter (\field -> length (filter isValidCoord field) == 20) fields

startShip :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
startShip start coords = map (`add` start) coords

putShowLn x = putStrLn $ show x

printTable x = putStrLn $ formatTable x

coordHit :: (Int, Int) -> PlayerMove
coordHit coord = Move coord Hit

step :: [(Int, Int)] -> String -> String -> IO ()
step field gameId player = do
  (responseCode, fileContents) <- getMessage player gameId
  putStrLn $ "responseCode: " ++ show responseCode ++ " -> '" ++ fileContents ++ "'"
  --fileContents <- readFile file
  let res = getValue $ apply jvalue fileContents
  let coord = convertCoord res
  let moves = getMoves coord
  let [player2, player1] = splitLists moves
  let currentHit = getCoord coord
  let isHit = if currentHit `elem` field then Hit else Miss
  let player1hits = [currentHit] ++ (map moveCoord player1)
  let leftFields = filter (`elem` player1hits) field
  r <- kitas player2
  let nextMove = head r
  if length leftFields == 20 then do
    putStrLn "GAME OVER"
    _ <- sendMessage (toJsonCoord (GameOver coord)) player gameId
    return $ ()
  else do
    let answer = (CoordPrev nextMove isHit coord)
    let textAnswer = toJsonCoord answer
    putStrLn textAnswer
    _ <- sendMessage textAnswer player gameId
    step field gameId player
  putStrLn $ show leftFields

step2 :: [(Int, Int)] -> String -> IO ()
step2 field fileContents = do
  let res = getValue $ apply jvalue fileContents
  let coord = convertCoord res
  let moves = getMoves coord
  let [player2, player1] = splitLists moves
  let currentHit = getCoord coord
  let isHit = if currentHit `elem` field then Hit else Miss
  let player1hits = [currentHit] ++ (map moveCoord player1)
  let leftFields = filter (`elem` player1hits) player1hits
  putStrLn $ "CIA: " ++ (show moves)
  r <- kitas player2
  let nextMove = head r
  if length leftFields == 20 then do
    putStrLn "GAME OVER"
    --_ <- sendMessage (toJsonCoord (GameOver coord)) player gameId
    return $ ()
  else do
    let answer = (CoordPrev nextMove isHit coord)
    let textAnswer = toJsonCoord answer
    putStrLn textAnswer

main :: IO ()
main = do
  field <- randomField
  putStrLn "ship field"
  printTable $ putMovesTable emptyTable $ map coordHit field
  args <- getArgs
  if length args == 2 then
    let [gameId, player] = args
    in step field gameId player
  else do
    let [file] = args
    content <- readFile file
    putStrLn content
    step2 field content
