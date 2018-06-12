{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Printer where

import qualified Data.Map      as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Set      as S
import           GHC.TypeLits
import           RuntimeSchema
import           Schema

class SchemaShow a where
    sShow :: a -> String

instance SchemaShow (CompleteSchema (Schema s p t l u)) where
    sShow (CompleteSchema schema) = sShow schema

instance SchemaShow (Schema s p t l u) where
    sShow (TrackCons t rest) = sShow t ++ "\n" ++ sShow rest
    sShow (SingleTurnoutCons singleTurnout rest) = sShow singleTurnout ++ "\n" ++ sShow rest
    sShow (StationEndCons stationEnd rest) = show stationEnd ++ "\n" ++ sShow rest
    sShow SNil               = "koniec."

instance KnownNat n => SchemaShow (Track n l1 l2) where
    sShow (Track _ len _ _) = replicate leftLen '─' ++ nrRep ++ replicate rightLen '─'
      where
        nrRep = show $ natVal $ Proxy @n
        leftLen = (len - length nrRep) `div` 2
        rightLen = len - leftLen - length nrRep

instance SchemaShow (Turnout td n) where
    sShow (Turnout direction) = unlines $ map show $ showTurnout [] direction 3 (natVal $ Proxy @n) Plus

instance SchemaShow (SingleTurnout l1 l2 l3) where
    sShow (SingleTurnoutLeftUp t _ _ _)    = sShow t
    sShow (SingleTurnoutRightUp t _ _ _)   = sShow t
    sShow (SingleTurnoutLeftDown t _ _ _)  = sShow t
    sShow (SingleTurnoutRightDown t _ _ _) = sShow t



