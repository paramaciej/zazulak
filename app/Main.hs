module Main where

import Schema
import Data.Typeable
import Printer

main :: IO ()
main = do
    print u
    print (typeOf u2)
    putStrLn $ sShow u2