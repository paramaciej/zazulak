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


signalBoxProgram :: M.Map Integer TurnoutState -> Maybe RuntimeFahrstrasse -> IO ()
signalBoxProgram turnoutStates mFahrstrasse = do
    clearScreen
    setCursorPosition 0 0

    putStrLn $ showRS turnoutStates mFahrstrasse $ getRuntimeSchema helSchema

    let availableFahrstrassen = M.fromList $ map (\fs@(RuntimeFahrstrasse nr _ _ _ _) -> (nr, fs))
                                [ getRuntimeFahrstrasse helFahr1
                                , getRuntimeFahrstrasse helFahr2
                                , getRuntimeFahrstrasse helFahr3
                                , getRuntimeFahrstrasse helFahr4
                                , getRuntimeFahrstrasse helFahr5
                                , getRuntimeFahrstrasse helFahr6
                                , getRuntimeFahrstrasse helFahr7
                                , getRuntimeFahrstrasse helFahr8
                                ]

    putStrLn $ intercalate "\n" $ map show $ M.elems availableFahrstrassen

    putStr "\n?>"
    hFlush stdout
    getLine >>= \case
        "q" -> putStrLn "Auf wiedersehen!"
        input -> case stripPrefix "r" input >>= (\nr -> readMaybe nr :: Maybe Integer) of
            Just nr -> if nr `elem` maybe [] blockedTurnouts mFahrstrasse
                then signalBoxProgram turnoutStates mFahrstrasse
                else signalBoxProgram (M.insert nr newVal turnoutStates) mFahrstrasse
              where
                newVal = case fromMaybe Plus (nr `M.lookup` turnoutStates) of
                                                         Plus  -> Minus
                                                         Minus -> Plus
            Nothing -> case stripPrefix "p" input >>= (\nr -> readMaybe nr :: Maybe Int) of
                Just fNr -> case mFahrstrasse of
                    Just (RuntimeFahrstrasse nr _ _ _ _) -> if fNr == nr then signalBoxProgram turnoutStates Nothing else signalBoxProgram turnoutStates mFahrstrasse
                    Nothing -> case fNr `M.lookup` availableFahrstrassen of
                        Nothing -> signalBoxProgram turnoutStates Nothing
                        Just fs@(RuntimeFahrstrasse _ _ fp fm _) ->
                            if all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Plus) fp && all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Minus) fm
                                then signalBoxProgram turnoutStates (Just fs)
                                else signalBoxProgram turnoutStates mFahrstrasse
                Nothing -> signalBoxProgram turnoutStates mFahrstrasse



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



    signalBoxProgram M.empty Nothing
