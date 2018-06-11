{-# LANGUAGE PolyKinds            #-}
-- {-# LANGUAGE TypeInType           #-}

-- {-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
-- {-# LANGUAGE KindSignatures       #-}
-- {-# LANGUAGE RankNTypes           #-}
-- {-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- {-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

-- data PointsType = LeftUp | RightUp | LeftDown | RightDown
-- data Rozjazd = Rozjazd Int PointsType
-- data PointState = Plus | Minus
--
--
--
-- showPoint :: Rozjazd -> PointState -> [String]
-- showPoint (Rozjazd _ RightUp) Plus =
--     [ " nr / "
--     , "------"
--     ]
-- showPoint (Rozjazd _ RightUp) Minus =
--     [ " nr / "
--     , "---/ -"]

-- --------------------

-- data Nat :: * where
--     Z :: Nat
--     S :: Nat -> Nat

import           Data.Kind     (Type)
import           Data.Typeable
import           GHC.TypeLits

data TurnoutDirection = LeftUp | RightUp | LeftDown | RightDown

data STurnoutDirection td where
    SLeftUp :: STurnoutDirection LeftUp
    SRightUp :: STurnoutDirection RightUp
    SLeftDown :: STurnoutDirection LeftDown
    SRightDown :: STurnoutDirection RightDown

deriving instance Show (STurnoutDirection td)

data TurnoutState = Plus | Minus

data Turnout (td :: TurnoutDirection) (n :: Nat) where
    Turnout :: KnownNat n => STurnoutDirection td -> Turnout td n

deriving instance Show (Turnout td n)

data LinkDirection = LeftLink | RightLink
data SLinkDirection ld where
    SLeftLink :: SLinkDirection LeftLink
    SRightLink :: SLinkDirection RightLink

deriving instance Show (SLinkDirection ld)

data Link (ld :: LinkDirection) (n :: Nat) where
    Link :: KnownNat n => SLinkDirection ld -> Link ld n

deriving instance Show (Link ld n)

ll :: KnownNat n => Link LeftLink n
ll = Link SLeftLink

lr :: KnownNat n => Link RightLink n
lr = Link SRightLink

data SingleTurnout l1 l2 l3 where
    SingleTurnoutLeftUp :: Turnout LeftUp n -> Link RightLink lIn -> Link LeftLink lPlus -> Link LeftLink lMinus ->
        SingleTurnout (Link RightLink lIn) (Link LeftLink lPlus) (Link LeftLink lMinus)
    SingleTurnoutRightUp :: Turnout RightUp n -> Link LeftLink lIn -> Link RightLink lPlus -> Link RightLink lMinus ->
        SingleTurnout (Link LeftLink lIn) (Link RightLink lPlus) (Link RightLink lMinus)
    SingleTurnoutLeftDown :: Turnout LeftDown n -> Link RightLink lIn -> Link LeftLink lPlus -> Link LeftLink lMinus ->
        SingleTurnout (Link RightLink lIn) (Link LeftLink lPlus) (Link LeftLink lMinus)
    SingleTurnoutRightDown :: Turnout RightDown n -> Link LeftLink lIn -> Link RightLink lPlus -> Link RightLink lMinus ->
        SingleTurnout (Link LeftLink lIn) (Link RightLink lPlus) (Link RightLink lMinus)

deriving instance Show (SingleTurnout l1 l2 l3)

data HalfTrapezium turnout1 turnout2 l1 l2 l3 l4 where
    HalfTrapeziumUp :: HalfTrapezium (Turnout RightUp n1) (Turnout LeftDown n2) (Link LeftLink ll1) (Link LeftLink ll2) (Link RightLink lr1) (Link RightLink lr2)
    HalfTrapeziumDown :: HalfTrapezium (Turnout RightDown n1) (Turnout LeftUp n2) (Link LeftLink ll1) (Link LeftLink ll2) (Link RightLink lr1) (Link RightLink lr2)

data StationEnd l where
    StationLeftEnd :: Link RightLink l -> StationEnd (Link RightLink l)
    StationRightEnd :: Link LeftLink l -> StationEnd (Link LeftLink l)

deriving instance Show (StationEnd l)

data Track (nr :: Nat) l1 l2 where
    Track :: KnownNat nr => Int -> Int -> Link RightLink l1 -> Link LeftLink l2 -> Track nr (Link RightLink l1) (Link LeftLink l2)

deriving instance Show (Track nr l1 l2)

type family In x xs where
    In _ '[] = 'False
    In x (x ':  xs) = 'True
    In x (_ ': xs) = In x xs


type family Remove x xs where
    Remove x '[] = TypeError (Text "Can't remove: " :<>: ShowType x :<>: Text " doesn't exist") -- TODO blabla
    Remove x (x ': xs) = xs
    Remove x (y ': xs) = y ': Remove x xs


data Schema :: [*] -> [*] -> [*] -> * where
    SNil :: Schema '[] '[] '[]
    SingleTurnoutCons :: (In l1 l ~ 'False, In l2 l ~ 'False, In l3 l ~ 'False) =>
        SingleTurnout l1 l2 l3 -> Schema s t l -> Schema ((SingleTurnout l1 l2 l3) ': s) t (l1 : l2 : l3 : l)
    TrackCons :: KnownNat n =>
        Track n l1 l2 -> Schema s t l -> Schema s ((Track n l1 l2) ': t) (Remove l1 (Remove l2 l))
    StationEndCons :: (In sel l ~ 'False) =>
        StationEnd sel -> Schema s t l -> Schema s t (sel ': l)

deriving instance Show (Schema s t l)

data CompleteSchema schema where
    CompleteSchema :: Schema s t '[] -> CompleteSchema (Schema s t '[])

deriving instance Show (CompleteSchema schema)
