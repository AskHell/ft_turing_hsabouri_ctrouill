module Main where

import Machine ( Machine, encode, eitherDecode )
import qualified Data.ByteString.Lazy as B

getOutput :: Either String Machine -> String
getOutput (Left s) = s
getOutput (Right m) = show $ encode m

main :: IO ()
main = do
  file_content <- B.readFile "machines/unary_sub.json"
  putStrLn $ getOutput $ eitherDecode file_content
