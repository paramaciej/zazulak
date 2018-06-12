{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Typeable
import           Hel
import           Printer
import           RuntimeFahrstrasse
import           RuntimeSchema
import           Schema
import           System.Console.ANSI
import           System.IO
import           Text.Read
import SignalBoxProgram

main :: IO ()
main = signalBoxProgram (getRuntimeSchema helSchema)
    [ getRuntimeFahrstrasse helFahr1
    , getRuntimeFahrstrasse helFahr2
    , getRuntimeFahrstrasse helFahr3
    , getRuntimeFahrstrasse helFahr4
    , getRuntimeFahrstrasse helFahr5
    , getRuntimeFahrstrasse helFahr6
    , getRuntimeFahrstrasse helFahr7
    , getRuntimeFahrstrasse helFahr8
    ]
