{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module Fahrstrasse where

import           Data.Typeable
import           Schema

data TeilweiseFahrstrasse :: * -> * -> [*] -> [*] -> [*] -> [*] -> * where -- todo Teilweise
-- completeSchema, starting semaphore, plusTurnouts, minusTurnouts, tracks, freeLinks
    FStart :: (In (Semaphore name sl1 sl2) s ~ 'True) =>
        CompleteSchema (Schema s p t l u) -> Semaphore name sl1 sl2
        -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) (Semaphore name sl1 sl2) '[] '[] '[] '[sl2] -- todo czy dobrze sl2?
    FTurnoutPlusCons ::
        ( In (SingleTurnout lIn lPlus lMinus) p ~ 'True
        , In (SingleTurnout lIn lPlus lMinus) fp ~ 'False
        , In (SingleTurnout lIn lPLus lMinus) fm ~ 'False
        ) => SingleTurnout lIn lPlus lMinus -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl
        -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem (SingleTurnout lIn lPlus lMinus : fp) fm ft (lIn : lPlus : fl)
    FTurnoutMinusCons ::
        ( In (SingleTurnout lIn lPlus lMinus) p ~ 'True
        , In (SingleTurnout lIn lPlus lMinus) fp ~ 'False
        , In (SingleTurnout lIn lPLus lMinus) fm ~ 'False
        ) => SingleTurnout lIn lPlus lMinus -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl
        -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp (SingleTurnout lIn lPlus lMinus : fm) ft (lIn : lMinus : fl)
    FTrackCons :: (In (Track n l1 l2) t ~ 'True, In (Track n l1 l2) ft ~ 'False) =>
        Track n l1 l2 -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl
        -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm (Track n l1 l2 : ft) (Remove l1 (Remove l2 fl))
    FStop :: (In (Link ld lnr) u ~ 'True, In (Link ld lnr) fl ~ 'False) => Link ld lnr -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl
        -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft (Link ld lnr : fl)

deriving instance Show (TeilweiseFahrstrasse schema sem fp fm ft fl)

data Fahrstrasse fahrstrasse where
    Fahrstrasse :: Int -> TeilweiseFahrstrasse schema sem fp fm ft '[] -> Fahrstrasse (TeilweiseFahrstrasse schema sem fp fm ft '[])

deriving instance Show (Fahrstrasse fahrstrasse)
