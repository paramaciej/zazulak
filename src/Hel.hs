{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Hel where

import           Fahrstrasse
import           Schema


helSchema
    = CompleteSchema
    $ TrackCons (Track 3 0 lr ll :: Track 0 (Link _ 19) (Link _ 20))
    $ TrackCons (Track 5 30 lr ll :: Track 7 (Link _ 25) (Link _ 37))
    $ TrackCons (Track 4 0 lr ll :: Track 0 (Link _ 38) (Link _ 40))
    $ TrackCons (Track 4 0 lr ll :: Track 0 (Link _ 24) (Link _ 51))
    $ TrackCons (Track 4 28 lr ll :: Track 5 (Link _ 52) (Link _ 36))
    $ TrackCons (Track 4 0 lr ll :: Track 0 (Link _ 22) (Link _ 23))
    $ TrackCons (Track 4 38 lr ll :: Track 25 (Link _ 7) (Link _ 21))
    $ TrackCons (Track 3 9 lr ll :: Track 0 (Link _ 28) (Link _ 49))
    $ TrackCons (Track 3 31 lr ll :: Track 3 (Link _ 50) (Link _ 34))
    $ TrackCons (Track 0 14 lr ll :: Track 8 (Link _ 10) (Link _ 11))
    $ TrackCons (Track 1 20 lr ll :: Track 4 (Link _ 46) (Link _ 32))
    $ TrackCons (Track 1 0 lr ll :: Track 0 (Link _ 31) (Link _ 45))
    $ TrackCons (Track 1 0 lr ll :: Track 0 (Link _ 13) (Link _ 14))
    $ TrackCons (Track 1 14 lr ll :: Track 6 (Link _ 9) (Link _ 12))
    $ TrackCons (Track 1 0 lr ll :: Track 0 (Link _ 4) (Link _ 8))
    $ TrackCons (Track 2 5 lr ll :: Track 0 (Link _ 41) (Link _ 42))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 35) (Link _ 39))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 30) (Link _ 47))
    $ TrackCons (Track 2 31 lr ll :: Track 1 (Link _ 48) (Link _ 33))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 27) (Link _ 29))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 18) (Link _ 26))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 16) (Link _ 17))
    $ TrackCons (Track 2 21 lr ll :: Track 0 (Link _ 6) (Link _ 53))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 54) (Link _ 15))
    $ TrackCons (Track 2 0 lr ll :: Track 0 (Link _ 3) (Link _ 5))
    $ TrackCons (Track 2 5 lr ll :: Track 0 (Link _ 44) (Link _ 2))
    $ TrackCons (Track 2 5 lr ll :: Track 0 (Link _ 1) (Link _ 43))
    $ SemaphoreCons helSemA $ SemaphoreCons helSemB $ SemaphoreCons helSemC $ SemaphoreCons helSemD $ SemaphoreCons helSemE $ SemaphoreCons helSemF
    $ SingleTurnoutCons helR24
    $ SingleTurnoutCons helR22
    $ SingleTurnoutCons helR23
    $ SingleTurnoutCons helR18
    $ SingleTurnoutCons helR14
    $ SingleTurnoutCons helR13
    $ SingleTurnoutCons helR15
    $ SingleTurnoutCons helR11
    $ SingleTurnoutCons helR10
    $ SingleTurnoutCons helR6
    $ SingleTurnoutCons helR9
    $ SingleTurnoutCons helR2
    $ SingleTurnoutCons helR1
    $ StationEndCons (StationRightEnd ll :: StationEnd (Link _ 42))
    $ StationEndCons (StationRightEnd ll :: StationEnd (Link _ 32))
    $ StationEndCons (StationLeftEnd lr :: StationEnd (Link _ 1))
      SNil

helSemA = SemaphoreRight ll lr :: Semaphore "A" (Link _ 43) (Link _ 44)
helSemB = SemaphoreRight ll lr :: Semaphore "B" (Link _ 53) (Link _ 54)
helSemC = SemaphoreLeft lr ll :: Semaphore "C" (Link _ 52) (Link _ 51)
helSemD = SemaphoreLeft lr ll :: Semaphore "D" (Link _ 50) (Link _ 49)
helSemE = SemaphoreLeft lr ll :: Semaphore "E" (Link _ 48) (Link _ 47)
helSemF = SemaphoreLeft lr ll :: Semaphore "F" (Link _ 46) (Link _ 45)

helR1  = SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 1)) ll lr lr :: SingleTurnout (Link _ 2) (Link _ 3) (Link _ 4)
helR2  = SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 2)) ll lr lr :: SingleTurnout (Link _ 8) (Link _ 9) (Link _ 10)
helR6  = SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 6)) ll lr lr :: SingleTurnout (Link _ 5) (Link _ 6) (Link _ 7)
helR9  = SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 9)) lr ll ll :: SingleTurnout (Link _ 13) (Link _ 12) (Link _ 11)
helR10 = SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 10)) lr ll ll :: SingleTurnout (Link _ 16) (Link _ 15) (Link _ 14)
helR11 = SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 11)) ll lr lr :: SingleTurnout (Link _ 17) (Link _ 18) (Link _ 19)
helR13 = SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 13)) ll lr lr :: SingleTurnout (Link _ 26) (Link _ 27) (Link _ 28)
helR14 = SingleTurnoutRightUp (Turnout SRightUp :: (Turnout _ 14)) ll lr lr :: SingleTurnout (Link _ 29) (Link _ 30) (Link _ 31)
helR15 = SingleTurnoutLeftUp (Turnout SLeftUp :: (Turnout _ 15)) lr ll ll :: SingleTurnout (Link _ 22) (Link _ 21) (Link _ 20)
helR18 = SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 18)) ll lr lr :: SingleTurnout (Link _ 23) (Link _ 24) (Link _ 25)
helR22 = SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 22)) lr ll ll :: SingleTurnout (Link _ 35) (Link _ 33) (Link _ 34)
helR23 = SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 23)) lr ll ll :: SingleTurnout (Link _ 38) (Link _ 36) (Link _ 37)
helR24 = SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 24)) lr ll ll :: SingleTurnout (Link _ 41) (Link _ 39) (Link _ 40)


helFahrA1 = Fahrstrasse $ FStop (lr :: Link _ 3) $ FTrackCons (Track 0 0 lr ll :: Track 0 (Link _ 44) (Link _ 2)) $ FTurnoutPlusCons helR1 $ FStart helSchema helSemA