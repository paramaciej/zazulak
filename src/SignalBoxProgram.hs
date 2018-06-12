{-# LANGUAGE LambdaCase #-}
module SignalBoxProgram where

import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           RuntimeFahrstrasse
import           RuntimeSchema
import           Schema
import           System.Console.ANSI
import           System.IO
import           Text.Read

signalBoxProgram :: RuntimeSchema -> [RuntimeFahrstrasse] -> IO ()
signalBoxProgram runtimeSchema initialFahrstrassen = mainLoop M.empty [] $ Right ""
  where
    availableFahrstrassen = M.fromList $ map (\fs@(RuntimeFahrstrasse nr _ _ _ _) -> (nr, fs)) initialFahrstrassen

    mainLoop :: M.Map Integer TurnoutState -> [RuntimeFahrstrasse] -> Either String String -> IO ()
    mainLoop turnoutStates fahrstrassen message = do
        clearScreen
        setCursorPosition 0 0

        putStrLn $ case message of
            Right txt -> setSGRCode [SetColor Foreground Vivid Green] ++ txt ++ setSGRCode []
            Left txt -> setSGRCode [SetColor Foreground Vivid Red] ++ txt ++ setSGRCode []

        putStrLn $ showRS turnoutStates fahrstrassen runtimeSchema
        putStrLn $ intercalate "\n" $ map show $ M.elems availableFahrstrassen

        putStr "\n?>"
        hFlush stdout
        getLine >>= \case
            "q" -> putStrLn "Auf wiedersehen!"
            input -> case stripPrefix "r" input >>= (\nr -> readMaybe nr :: Maybe Integer) of
                Just nr -> if nr `elem` concatMap blockedTurnouts fahrstrassen
                    then mainLoop turnoutStates fahrstrassen $ Left $ "Turnout " ++ show nr ++ " blocked!"
                    else mainLoop (M.insert nr newVal turnoutStates) fahrstrassen $ Right $ "Turnout " ++ show nr ++ " switched."
                  where
                    newVal = case fromMaybe Plus (nr `M.lookup` turnoutStates) of
                                                             Plus  -> Minus
                                                             Minus -> Plus
                Nothing -> case stripPrefix "p" input >>= (\nr -> readMaybe nr :: Maybe Int) of
                    Just fNr ->
                        if fNr `elem` map fahrstrasseNr fahrstrassen then
                            mainLoop turnoutStates (filter (\fs -> fNr /= fahrstrasseNr fs) fahrstrassen) $ Right $ "Fahrstraße " ++ show fNr ++ " deactivated."
                        else case fNr `M.lookup` availableFahrstrassen of
                            Just fs@(RuntimeFahrstrasse _ sem fp fm ft) -> if fpOk && fmOk && allFreeTurnouts && semOk && trackOk
                                then mainLoop turnoutStates (fs : fahrstrassen) $ Right $ "Fahrstraße " ++ show fNr ++ " activated."
                                else mainLoop turnoutStates fahrstrassen $ Left $ "Cannot activate fahrstraße " ++ show fNr ++ " (blocked)!"
                              where
                                fpOk = all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Plus) fp
                                fmOk = all (\x -> fromMaybe Plus (x `M.lookup` turnoutStates) == Minus) fm
                                allFreeTurnouts = null $ concatMap blockedTurnouts fahrstrassen `intersect` (fp ++ fm)
                                semOk = sem `notElem` map (\(RuntimeFahrstrasse _ s _ _ _) -> s) fahrstrassen
                                trackOk = null $ concatMap (\(RuntimeFahrstrasse _ _ _ _ ts) -> ts) fahrstrassen `intersect` ft
                            Nothing -> mainLoop turnoutStates fahrstrassen $ Right $ "Fahrstraße " ++ show fNr ++ " does not exist!"
                    Nothing -> mainLoop turnoutStates fahrstrassen $ Left "Wrong option; available: r{nr}, p{nr}."



