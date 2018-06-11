{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-#OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RuntimeFahrstrasse where

import           Data.Proxy
import           Fahrstrasse
import           GHC.TypeLits
import           RuntimeSchema
import           Schema

data RuntimeFahrstrasse = RuntimeFahrstrasse RuntimeSchema String [Integer] [Integer] [(RRightLink, RLeftLink)] deriving Show


class RuntimeableFahrstrasse a where
    getRuntimeFahrstrasse :: a -> RuntimeFahrstrasse

instance RuntimeableFahrstrasse (Fahrstrasse (TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl)) where
    getRuntimeFahrstrasse (Fahrstrasse teilweise) = getRuntimeFahrstrasse' teilweise

class RuntimeableInternalFahrstrasse a where
    getRuntimeFahrstrasse' :: a -> RuntimeFahrstrasse

instance RuntimeableInternalFahrstrasse (TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl) where
    getRuntimeFahrstrasse' = aux (RuntimeFahrstrasse (RuntimeSchema [] [] [] []) "panic" [] [] [])
      where
        aux :: forall s p t l u sem fp fm ft fl. RuntimeFahrstrasse -> TeilweiseFahrstrasse (CompleteSchema (Schema s p t l u)) sem fp fm ft fl -> RuntimeFahrstrasse
        aux (RuntimeFahrstrasse _ _ fp fm ft) (FStart schema ((SemaphoreLeft l1 l2) :: Semaphore name _ _)) =
            RuntimeFahrstrasse (getRuntimeSchema schema) (symbolVal $ Proxy @name) fp fm ft
        aux (RuntimeFahrstrasse _ _ fp fm ft) (FStart schema ((SemaphoreRight l1 l2) :: Semaphore name _ _)) =
            RuntimeFahrstrasse (getRuntimeSchema schema) (symbolVal $ Proxy @name) fp fm ft
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutPlusCons (SingleTurnoutLeftUp (Turnout SLeftUp :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem (natVal (Proxy @n) : fp) fm ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutPlusCons (SingleTurnoutLeftDown (Turnout SLeftDown :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem (natVal (Proxy @n) : fp) fm ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutPlusCons (SingleTurnoutRightUp (Turnout SRightUp :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem (natVal (Proxy @n) : fp) fm ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutPlusCons (SingleTurnoutRightDown (Turnout SRightDown :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem (natVal (Proxy @n) : fp) fm ft) rest

        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutMinusCons (SingleTurnoutLeftUp (Turnout SLeftUp :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem fp (natVal (Proxy @n) : fm) ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutMinusCons (SingleTurnoutLeftDown (Turnout SLeftDown :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem fp (natVal (Proxy @n) : fm) ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutMinusCons (SingleTurnoutRightUp (Turnout SRightUp :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem fp (natVal (Proxy @n) : fm) ft) rest
        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTurnoutMinusCons (SingleTurnoutRightDown (Turnout SRightDown :: Turnout _ n) l1 l2 l3) rest) =
            aux (RuntimeFahrstrasse schema sem fp (natVal (Proxy @n) : fm) ft) rest

        aux (RuntimeFahrstrasse schema sem fp fm ft) (FTrackCons (Track _ _ (Link SRightLink :: Link _ l1) (Link SLeftLink :: Link _ l2) :: Track _ _ _) rest) =
            aux (RuntimeFahrstrasse schema sem fp fm ((RRightLink $ natVal $ Proxy @l1, RLeftLink $ natVal $ Proxy @l2) : ft)) rest

        aux runtime (FStop _ rest) = aux runtime rest
