module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs, getProgName)
--import System.Directory
import System.IO
import Data.List
import Text.Printf (printf)

import Machine ( Machine, encode, eitherDecode )

getOutput :: Either String Machine -> String
getOutput (Left s) = s
getOutput (Right m) = show $ encode m

usage :: IO ()
usage = putStrLn "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n  jsonfile\t\tjson description of the machine\n  input\t\t\tinput of the machine\n\noptional arguments:\n  -h, --help\t\tshow this help message and exit"

display_error :: String -> IO ()
display_error s = putStrLn $ printf "\x1b[31mError\x1b[0m: %s" s

dispatch :: [String] -> IO ()
dispatch (help : _)
    | help == "-h" || help == "--help" =
        usage
dispatch (machine : input : _)
    | (not $ machine == []) && (not $ input == []) = do -- when both are valid
        file_content <- B.readFile machine
        putStrLn $ getOutput $ eitherDecode file_content
    | machine == [] =
        display_error "jsonfile is an empty string"
    | input == [] =
        display_error "input is an empty string"
dispatch _ =
    usage

main = do
    args <- getArgs
    dispatch args
