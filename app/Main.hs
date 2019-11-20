module Main where

import Lib
import Control.Lens
import Network.Wreq
import Parser(apply, JsonValue, jvalue, apply, getValue)
import qualified Data.ByteString.Lazy.Char8 as B

{-
main :: IO ()
main = do
  r <- get "https://bentkus.eu"
  putStrLn $ show r
  putStrLn $ show $ r ^. responseBody
-}

main :: IO()
main = do
  putStrLn $ show $ apply jvalue "1"
  putStrLn $ show $ apply jvalue "\"asd\""
