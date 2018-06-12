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
import           Jastarnia
import           Printer
import           RuntimeFahrstrasse
import           RuntimeSchema
import           Schema
import           System.Console.ANSI
import           System.IO
import           Text.Read


signalBoxProgram :: M.Map Integer TurnoutState -> [RuntimeFahrstrasse] -> IO ()
signalBoxProgram turnoutStates fahrstrassen = do
    clearScreen
    setCursorPosition 0 0

    putStrLn $ showRS turnoutStates fahrstrassen $ getRuntimeSchema jastarniaSchema

    let availableFahrstrassen = M.fromList $ map (\fs@(RuntimeFahrstrasse nr _ _ _ _) -> (nr, fs))
                                [ getRuntimeFahrstrasse jastarniaFahr1
                                , getRuntimeFahrstrasse jastarniaFahr2
                                , getRuntimeFahrstrasse jastarniaFahr3
                                , getRuntimeFahrstrasse jastarniaFahr4
                                , getRuntimeFahrstrasse jastarniaFahr5
                                , getRuntimeFahrstrasse jastarniaFahr6
                                , getRuntimeFahrstrasse jastarniaFahr7
                                , getRuntimeFahrstrasse jastarniaFahr8
                                ]

    putStrLn $ intercalate "\n" $ map show $ M.elems availableFahrstrassen

    putStr "\n?>"
    hFlush stdout
    getLine >>= \case
        "q" -> putStrLn "Auf wiedersehen!"
        input -> case stripPrefix "r" input >>= (\nr -> readMaybe nr :: Maybe Integer) of
            Just nr -> if nr `elem` concatMap blockedTurnouts fahrstrassen
                then signalBoxProgram turnoutStates fahrstrassen
                else signalBoxProgram (M.insert nr newVal turnoutStates) fahrstrassen
              where
                newVal = case fromMaybe Plus (nr `M.lookup` turnoutStates) of
                                                         Plus  -> Minus
                                                         Minus -> Plus
            Nothing -> case stripPrefix "p" input >>= (\nr -> readMaybe nr :: Maybe Int) of
                Just fNr ->
                    if fNr `elem` map fahrstrasseNr fahrstrassen then
                        signalBoxProgram turnoutStates (filter (\fs -> fNr /= fahrstrasseNr fs) fahrstrassen)
                    else case fNr `M.lookup` availableFahrstrassen of
                        Nothing -> signalBoxProgram turnoutStates fahrstrassen
                        Just fs@(RuntimeFahrstrasse _ sem fp fm ft) -> if fpOk && fmOk && allFreeTurnouts && semOk && trackOk
                            then signalBoxProgram turnoutStates (fs : fahrstrassen)
                            else signalBoxProgram turnoutStates fahrstrassen
                          where
                            fpOk = all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Plus) fp
                            fmOk = all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Minus) fm
                            allFreeTurnouts = null $ concatMap blockedTurnouts fahrstrassen `intersect` (fp ++ fm)
                            semOk = sem `notElem` map (\(RuntimeFahrstrasse _ s _ _ _) -> s) fahrstrassen
                            trackOk = null $ concatMap (\(RuntimeFahrstrasse _ _ _ _ ts) -> ts) fahrstrassen `intersect` ft



main :: IO ()
main = signalBoxProgram M.empty []
