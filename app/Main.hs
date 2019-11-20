module Main where

import Lib

import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  r <- get "https://bentkus.eu"
  putStrLn $ show r
  putStrLn $ show $ r ^. responseBody
