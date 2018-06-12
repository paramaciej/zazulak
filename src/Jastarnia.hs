{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Jastarnia where

import           Fahrstrasse
import           Schema

jastarniaT1 = Track 1 30 lr ll :: Track 1 (Link _ 8) (Link _ 11)
jastarniaT3 = Track 2 30 lr ll :: Track 3 (Link _ 10) (Link _ 13)

jastarniaJ01 = Track 1 5 lr ll :: Track 0 (Link _ 1) (Link _ 2)
jastarniaJ03 = Track 1 10 lr ll :: Track 0 (Link _ 3) (Link _ 4)
jastarniaJ05 = Track 1 0 lr ll :: Track 0 (Link _ 5) (Link _ 7)
jastarniaJ06 = Track 2 0 lr ll :: Track 0 (Link _ 6) (Link _ 9)
jastarniaJ12 = Track 1 0 lr ll :: Track 0 (Link _ 12) (Link _ 15)
jastarniaJ14 = Track 2 0 lr ll :: Track 0 (Link _ 14) (Link _ 17)
jastarniaJ16 = Track 1 10 lr ll :: Track 0 (Link _ 16) (Link _ 18)
jastarniaJ19 = Track 1 5 lr ll :: Track 0 (Link _ 19) (Link _ 20)

jastarniaR1 = SingleTurnoutRightDown (Turnout SRightDown :: (Turnout _ 1)) ll lr lr :: SingleTurnout (Link _ 4) (Link _ 5) (Link _ 6)
jastarniaR4 = SingleTurnoutLeftDown (Turnout SLeftDown :: (Turnout _ 4)) lr ll ll :: SingleTurnout (Link _ 16) (Link _ 15) (Link _ 17)

jastarniaSemA = SemaphoreRight ll lr :: Semaphore "A" (Link _ 2) (Link _ 3)
jastarniaSemD = SemaphoreRight ll lr :: Semaphore "D" (Link _ 11) (Link _ 12)
jastarniaSemE = SemaphoreRight ll lr :: Semaphore "E" (Link _ 13) (Link _ 14)

jastarniaSemB = SemaphoreLeft lr ll :: Semaphore "B" (Link _ 10) (Link _ 9)
jastarniaSemC = SemaphoreLeft lr ll :: Semaphore "C" (Link _ 8) (Link _ 7)
jastarniaSemF = SemaphoreLeft lr ll :: Semaphore "F" (Link _ 19) (Link _ 18)

jastarniaSchema
    = CompleteSchema
    $ TrackCons jastarniaT1 $ TrackCons jastarniaT3
    $ TrackCons jastarniaJ01 $ TrackCons jastarniaJ03 $ TrackCons jastarniaJ05
    $ TrackCons jastarniaJ06 $ TrackCons jastarniaJ12 $ TrackCons jastarniaJ14
    $ TrackCons jastarniaJ16 $ TrackCons jastarniaJ19
    $ SemaphoreCons jastarniaSemA $ SemaphoreCons jastarniaSemB
    $ SemaphoreCons jastarniaSemC $ SemaphoreCons jastarniaSemD
    $ SemaphoreCons jastarniaSemE $ SemaphoreCons jastarniaSemF
    $ SingleTurnoutCons jastarniaR1 $ SingleTurnoutCons jastarniaR4
    $ StationEndCons (StationRightEnd ll :: StationEnd (Link _ 20))
    $ StationEndCons (StationLeftEnd lr :: StationEnd (Link _ 1))
      SNil

jastarniaFahr1 = Fahrstrasse 1
    $ FTrackCons jastarniaJ03 $ FTrackCons jastarniaJ05
    $ FTrackCons jastarniaT1
    $ FStop (ll :: Link _ 11)
    $ FStop (ll :: Link _ 7) $ FStop (lr :: Link _ 8)
    $ FTurnoutPlusCons jastarniaR1
    $ FStart jastarniaSchema jastarniaSemA


jastarniaFahr2 = Fahrstrasse 2
    $ FTrackCons jastarniaJ03 $ FTrackCons jastarniaJ06
    $ FTrackCons jastarniaT3
    $ FStop (ll :: Link _ 13)
    $ FStop (ll :: Link _ 9) $ FStop (lr :: Link _ 10)
    $ FTurnoutMinusCons jastarniaR1
    $ FStart jastarniaSchema jastarniaSemA

jastarniaFahr3 = Fahrstrasse 3
    $ FTrackCons jastarniaJ12 $ FTrackCons jastarniaJ16
    $ FTrackCons jastarniaT1
    $ FStop (lr :: Link _ 8)
    $ FStop (ll :: Link _ 11) $ FStop (lr :: Link _ 12)
    $ FTurnoutPlusCons jastarniaR4
    $ FStart jastarniaSchema jastarniaSemF

jastarniaFahr4 = Fahrstrasse 4
    $ FTrackCons jastarniaJ14 $ FTrackCons jastarniaJ16
    $ FTrackCons jastarniaT3
    $ FStop (lr :: Link _ 10)
    $ FStop (ll :: Link _ 13) $ FStop (lr :: Link _ 14)
    $ FTurnoutMinusCons jastarniaR4
    $ FStart jastarniaSchema jastarniaSemF

jastarniaFahr5 = Fahrstrasse 11
    $ FTrackCons jastarniaJ01 $ FTrackCons jastarniaJ03 $ FTrackCons jastarniaJ06
    $ FStop (lr :: Link _ 1)
    $ FStop (ll :: Link _ 2) $ FStop (lr :: Link _ 3)
    $ FTurnoutMinusCons jastarniaR1
    $ FStart jastarniaSchema jastarniaSemB

jastarniaFahr6 = Fahrstrasse 12
    $ FTrackCons jastarniaJ01 $ FTrackCons jastarniaJ03 $ FTrackCons jastarniaJ05
    $ FStop (lr :: Link _ 1)
    $ FStop (ll :: Link _ 2) $ FStop (lr :: Link _ 3)
    $ FTurnoutPlusCons jastarniaR1
    $ FStart jastarniaSchema jastarniaSemC

jastarniaFahr7 = Fahrstrasse 13
    $ FTrackCons jastarniaJ12 $ FTrackCons jastarniaJ16 $ FTrackCons jastarniaJ19
    $ FStop (ll :: Link _ 20)
    $ FStop (ll :: Link _ 18) $ FStop (lr :: Link _ 19)
    $ FTurnoutPlusCons jastarniaR4
    $ FStart jastarniaSchema jastarniaSemD

jastarniaFahr8 = Fahrstrasse 14
    $ FTrackCons jastarniaJ14 $ FTrackCons jastarniaJ16 $ FTrackCons jastarniaJ19
    $ FStop (ll :: Link _ 20)
    $ FStop (ll :: Link _ 18) $ FStop (lr :: Link _ 19)
    $ FTurnoutMinusCons jastarniaR4
    $ FStart jastarniaSchema jastarniaSemE