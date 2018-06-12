{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Hel where

import           Fahrstrasse
import           Schema

helT1 = Track 2 31 lr ll :: Track 1 (Link _ 48) (Link _ 33)
helT3 = Track 3 31 lr ll :: Track 3 (Link _ 50) (Link _ 34)
helT5 = Track 4 28 lr ll :: Track 5 (Link _ 52) (Link _ 36)
helT7 = Track 5 30 lr ll :: Track 7 (Link _ 25) (Link _ 37)
helT25 = Track 4 38 lr ll :: Track 25 (Link _ 7) (Link _ 21)
helT4 = Track 1 20 lr ll :: Track 4 (Link _ 46) (Link _ 32)
helT6 = Track 1 14 lr ll :: Track 6 (Link _ 9) (Link _ 12)
helT8 = Track 0 14 lr ll :: Track 8 (Link _ 10) (Link _ 11)


helJ19 = Track 3 0 lr ll :: Track 0 (Link _ 19) (Link _ 20)
helJ38 = Track 4 0 lr ll :: Track 0 (Link _ 38) (Link _ 40)
helJ24 = Track 4 0 lr ll :: Track 0 (Link _ 24) (Link _ 51)
helJ22 = Track 4 0 lr ll :: Track 0 (Link _ 22) (Link _ 23)
helJ28 = Track 3 9 lr ll :: Track 0 (Link _ 28) (Link _ 49)
helJ31 = Track 1 0 lr ll :: Track 0 (Link _ 31) (Link _ 45)
helJ13 = Track 1 0 lr ll :: Track 0 (Link _ 13) (Link _ 14)
helJ04 = Track 1 0 lr ll :: Track 0 (Link _ 4) (Link _ 8)
helJ41 = Track 2 5 lr ll :: Track 0 (Link _ 41) (Link _ 42)
helJ35 = Track 2 0 lr ll :: Track 0 (Link _ 35) (Link _ 39)
helJ30 = Track 2 0 lr ll :: Track 0 (Link _ 30) (Link _ 47)
helJ27 = Track 2 0 lr ll :: Track 0 (Link _ 27) (Link _ 29)
helJ18 = Track 2 0 lr ll :: Track 0 (Link _ 18) (Link _ 26)
helJ16 = Track 2 0 lr ll :: Track 0 (Link _ 16) (Link _ 17)
helJ06 = Track 2 21 lr ll :: Track 0 (Link _ 6) (Link _ 53)
helJ54 = Track 2 0 lr ll :: Track 0 (Link _ 54) (Link _ 15)
helJ03 = Track 2 0 lr ll :: Track 0 (Link _ 3) (Link _ 5)
helJ44 = Track 2 5 lr ll :: Track 0 (Link _ 44) (Link _ 2)
helJ01 = Track 2 5 lr ll :: Track 0 (Link _ 1) (Link _ 43)

helSchema
    = CompleteSchema
    $ TrackCons helT1 $ TrackCons helT3 $ TrackCons helT5 $ TrackCons helT7 $ TrackCons helT25 $ TrackCons helT4 $ TrackCons helT6 $ TrackCons helT8
    $ TrackCons helJ19
    $ TrackCons helJ38
    $ TrackCons helJ24
    $ TrackCons helJ22
    $ TrackCons helJ28
    $ TrackCons helJ31
    $ TrackCons helJ13
    $ TrackCons helJ04
    $ TrackCons helJ41
    $ TrackCons helJ35
    $ TrackCons helJ30
    $ TrackCons helJ27
    $ TrackCons helJ18
    $ TrackCons helJ16
    $ TrackCons helJ06
    $ TrackCons helJ54
    $ TrackCons helJ03
    $ TrackCons helJ44
    $ TrackCons helJ01
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


helFahr1 = Fahrstrasse 1
    $ FTrackCons helJ06
    $ FStop (ll :: Link _ 53)
    $ FTrackCons helJ03
    $ FTrackCons helJ44
    $ FTurnoutPlusCons helR6
    $ FTurnoutPlusCons helR1
    $ FStart helSchema helSemA

helFahr2 = Fahrstrasse 2
    $ FTrackCons helT1
    $ FTrackCons helJ30
    $ FTrackCons helJ27
    $ FTrackCons helJ18
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FStop (ll :: Link _ 33)
    $ FStop (ll :: Link _ 47)
    $ FStop (lr :: Link _ 48)
    $ FTurnoutPlusCons helR14
    $ FTurnoutPlusCons helR13
    $ FTurnoutPlusCons helR11
    $ FTurnoutPlusCons helR10
    $ FStart helSchema helSemB

helFahr3 = Fahrstrasse 3
    $ FTrackCons helT3
    $ FTrackCons helJ28
    $ FTrackCons helJ18
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FStop (ll :: Link _ 34)
    $ FStop (ll :: Link _ 49)
    $ FStop (lr :: Link _ 50)
    $ FTurnoutMinusCons helR13
    $ FTurnoutPlusCons helR11
    $ FTurnoutPlusCons helR10
    $ FStart helSchema helSemB


helFahr4 = Fahrstrasse 4
    $ FTrackCons helT4
    $ FTrackCons helJ31
    $ FTrackCons helJ27
    $ FTrackCons helJ18
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FStop (ll :: Link _ 32)
    $ FStop (ll :: Link _ 45)
    $ FStop (lr :: Link _ 46)
    $ FTurnoutMinusCons helR14
    $ FTurnoutPlusCons helR13
    $ FTurnoutPlusCons helR11
    $ FTurnoutPlusCons helR10
    $ FStart helSchema helSemB

helFahr5 = Fahrstrasse 5
    $ FTrackCons helT5
    $ FTrackCons helJ24
    $ FTrackCons helJ22
    $ FTrackCons helJ19
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FStop (ll :: Link _ 36)
    $ FStop (ll :: Link _ 51)
    $ FStop (lr :: Link _ 52)
    $ FTurnoutPlusCons helR18
    $ FTurnoutMinusCons helR15
    $ FTurnoutMinusCons helR11
    $ FTurnoutPlusCons helR10
    $ FStart helSchema helSemB

helFahr6 = Fahrstrasse 6
    $ FTrackCons helJ30
    $ FTrackCons helJ27
    $ FTrackCons helJ18
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FTrackCons helJ06
    $ FTrackCons helJ03
    $ FTrackCons helJ44
    $ FTrackCons helJ01
    $ FStop (lr :: Link _ 1)
    $ FStop (ll :: Link _ 43)
    $ FStop (lr :: Link _ 44)
    $ FStop (ll :: Link _ 53)
    $ FStop (lr :: Link _ 54)
    $ FTurnoutPlusCons helR14
    $ FTurnoutPlusCons helR13
    $ FTurnoutPlusCons helR11
    $ FTurnoutPlusCons helR10
    $ FTurnoutPlusCons helR6
    $ FTurnoutPlusCons helR1
    $ FStart helSchema helSemE


helFahr7 = Fahrstrasse 7
    $ FTrackCons helJ24
    $ FTrackCons helJ22
    $ FTrackCons helJ19
    $ FTrackCons helJ16
    $ FTrackCons helJ54
    $ FTrackCons helJ06
    $ FTrackCons helJ03
    $ FTrackCons helJ44
    $ FTrackCons helJ01
    $ FStop (lr :: Link _ 1)
    $ FStop (ll :: Link _ 43)
    $ FStop (lr :: Link _ 44)
    $ FStop (ll :: Link _ 53)
    $ FStop (lr :: Link _ 54)
    $ FTurnoutPlusCons helR18
    $ FTurnoutMinusCons helR15
    $ FTurnoutMinusCons helR11
    $ FTurnoutPlusCons helR10
    $ FTurnoutPlusCons helR6
    $ FTurnoutPlusCons helR1
    $ FStart helSchema helSemC

helFahr8 = Fahrstrasse 8
    $ FTrackCons helJ24
    $ FTrackCons helJ22
    $ FTrackCons helT25
    $ FTrackCons helJ03
    $ FTrackCons helJ44
    $ FTrackCons helJ01
    $ FStop (lr :: Link _ 1)
    $ FStop (ll :: Link _ 43)
    $ FStop (lr :: Link _ 44)
    $ FTurnoutPlusCons helR18
    $ FTurnoutPlusCons helR15
    $ FTurnoutMinusCons helR6
    $ FTurnoutPlusCons helR1
    $ FStart helSchema helSemC