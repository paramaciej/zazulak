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
import           RuntimeSchema
import           Schema
import           System.Console.ANSI
import           System.IO
import           Text.Read


signalBoxProgram :: M.Map Integer TurnoutState -> IO ()
signalBoxProgram turnoutStates = do
    clearScreen
    setCursorPosition 0 0

    putStrLn $ showRS turnoutStates $ getRuntimeSchema helSchema

    putStr "\n?>"
    hFlush stdout
    getLine >>= \case
        "q" -> putStrLn "Auf wiedersehen!"
        input -> case stripPrefix "r" input >>= (\nr -> readMaybe nr :: Maybe Integer) of
            Just nr  -> signalBoxProgram (M.insert nr newVal turnoutStates)
              where
                newVal = case fromMaybe Plus (nr `M.lookup` turnoutStates) of
                                                         Plus  -> Minus
                                                         Minus -> Plus
            Nothing -> signalBoxProgram turnoutStates



main :: IO ()
main = do

    let test1 = StationEndCons (StationLeftEnd lr :: StationEnd (Link RightLink 1)) SNil
    let test2 = SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout LeftUp 5)) lr ll ll :: SingleTurnout (Link _ 2) (Link _ 3) (Link _ 4)) test1
    let test3 = StationEndCons (StationRightEnd ll :: StationEnd (Link LeftLink 5)) test2

    print (typeOf test3)
    putStrLn $ sShow test3


    let ultimate    = CompleteSchema
                    $ TrackCons (Track 1 50 lr ll :: Track 2 (Link RightLink 4) (Link LeftLink 5))
                    $ TrackCons (Track 2 50 lr ll :: Track 1 (Link RightLink 3) (Link LeftLink 6))
                    $ TrackCons (Track 2 10 lr ll :: Track 0 (Link RightLink 7) (Link LeftLink 8))
                    $ TrackCons (Track 2 10 lr ll :: Track 0 (Link RightLink 1) (Link LeftLink 2))
                    $ SingleTurnoutCons (SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 1)) ll lr lr :: SingleTurnout (Link LeftLink 2) (Link RightLink 3) (Link RightLink 4))
                    $ SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 2)) lr ll ll :: SingleTurnout (Link RightLink 7) (Link LeftLink 6) (Link LeftLink 5))
                    $ StationEndCons (StationRightEnd ll :: StationEnd (Link LeftLink 8))
                    $ StationEndCons (StationLeftEnd lr :: StationEnd (Link RightLink 1))
                      SNil

    print ultimate

    putStrLn $ sShow ultimate

    let x = getRuntimeSchema ultimate

    print x
    print $ allLinks x
    putStrLn $ showRS (M.singleton 1 Minus) x


    signalBoxProgram M.empty
