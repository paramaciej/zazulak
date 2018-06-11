{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Hel where

import           Schema


helSchema
    = CompleteSchema
    $ TrackCons (Track 3 0 lr ll :: Track 0 (Link _ 19) (Link _ 20))
    $ TrackCons (Track 5 30 lr ll :: Track 7 (Link _ 25) (Link _ 37))
    $ TrackCons (Track 4 0 lr ll :: Track 0 (Link _ 38) (Link _ 40))
    $ TrackCons (Track 4 30 lr ll :: Track 5 (Link _ 24) (Link _ 36))
    $ TrackCons (Track 4 0 lr ll :: Track 0 (Link _ 22) (Link _ 23))
    $ TrackCons (Track 4 38 lr ll :: Track 25 (Link _ 7) (Link _ 21))
    $ TrackCons (Track 3 42 lr ll :: Track 3 (Link _ 28) (Link _ 34))
    $ TrackCons (Track 0 14 lr ll :: Track 8 (Link _ 10) (Link _ 11))
    $ TrackCons (Track 1 20 lr ll :: Track 4 (Link _ 31) (Link _ 32))
    $ TrackCons (Track 1 0 lr ll :: Track 0 (Link _ 13) (Link _ 14))
    $ TrackCons (Track 1 14 lr ll :: Track 6 (Link _ 9) (Link _ 12))
    $ TrackCons (Track 1 0 lr ll :: Track 0 (Link _ 4) (Link _ 8))
    $ TrackCons (Track 2 5 lr ll :: Track 0 (Link _ 41) (Link _ 42))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 35) (Link _ 39))
    $ TrackCons (Track 2 33 lr ll :: Track 1 (Link _ 30) (Link _ 33))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 27) (Link _ 29))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 18) (Link _ 26))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 16) (Link _ 17))
    $ TrackCons (Track 2 23 lr ll :: Track 0 (Link _ 6) (Link _ 15))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 3) (Link _ 5))
    $ TrackCons (Track 2 5 lr ll :: Track 0 (Link _ 1) (Link _ 2))
    $ SingleTurnoutCons (SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 24)) lr ll ll :: SingleTurnout (Link _ 41) (Link _ 39) (Link _ 40))
    $ SingleTurnoutCons (SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 22)) lr ll ll :: SingleTurnout (Link _ 35) (Link _ 33) (Link _ 34))
    $ SingleTurnoutCons (SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 23)) lr ll ll :: SingleTurnout (Link _ 38) (Link _ 36) (Link _ 37))
    $ SingleTurnoutCons (SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 18)) ll lr lr :: SingleTurnout (Link _ 23) (Link _ 24) (Link _ 25))
    $ SingleTurnoutCons (SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 14)) ll lr lr :: SingleTurnout (Link _ 29) (Link _ 30) (Link _ 31))
    $ SingleTurnoutCons (SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 13)) ll lr lr :: SingleTurnout (Link _ 26) (Link _ 27) (Link _ 28))
    $ SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 15)) lr ll ll :: SingleTurnout (Link _ 22) (Link _ 21) (Link _ 20))
    $ SingleTurnoutCons (SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 11)) ll lr lr :: SingleTurnout (Link _ 17) (Link _ 18) (Link _ 19))
    $ SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 10)) lr ll ll :: SingleTurnout (Link _ 16) (Link _ 15) (Link _ 14))
    $ SingleTurnoutCons (SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 6)) ll lr lr :: SingleTurnout (Link _ 5) (Link _ 6) (Link _ 7))
    $ SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 9)) lr ll ll :: SingleTurnout (Link _ 13) (Link _ 12) (Link _ 11))
    $ SingleTurnoutCons (SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 2)) ll lr lr :: SingleTurnout (Link _ 8) (Link _ 9) (Link _ 10))
    $ SingleTurnoutCons (SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 1)) ll lr lr :: SingleTurnout (Link _ 2) (Link _ 3) (Link _ 4))
    $ StationEndCons (StationRightEnd ll :: StationEnd (Link _ 42))
    $ StationEndCons (StationRightEnd ll :: StationEnd (Link _ 32))
    $ StationEndCons (StationLeftEnd lr :: StationEnd (Link _ 1))
      SNil
