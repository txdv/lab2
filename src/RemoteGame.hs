module RemoteGame where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit

sendMessage :: String -> String -> String -> IO Int
sendMessage message player gameId = do
  let request
        = setRequestMethod (BS.pack "POST")
        $ setRequestHeader (CI.mk $ BS.pack "Content-Type") [BS.pack contentType]
        $ setRequestBody (RequestBodyBS $ BS.pack message)
        $ baseRequest player gameId
  response <- httpBS request
  let code = getResponseStatusCode response
  putStrLn ("answer:" ++ (BS.unpack $ getResponseBody response))
  return code

getMessage :: String -> String -> IO (Int, String)
getMessage player gameId = do
  let request
        = setRequestMethod (BS.pack "GET")
        $ setRequestHeader (CI.mk $ BS.pack "Accept") [BS.pack contentType]
        $ baseRequest player gameId
  response <- httpBS request
  let code = getResponseStatusCode response
  return (code, BS.unpack $ getResponseBody response)

contentType :: String
contentType = "application/json"

baseRequest :: String -> String -> Request
baseRequest player gameId
  = setRequestPath (BS.pack $ "/game/" ++ gameId ++ "/player/" ++ player)
  $ setRequestHost (BS.pack url)
  $ defaultRequest
  where
    url = "battleship.haskell.lt"

