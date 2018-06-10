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

data Turnout (td :: TurnoutDirection) (n :: Nat) = Turnout

data LinkDirection = LeftLink | RightLink

data Link (ld :: LinkDirection) (n :: Nat) = Link

data SingleTurnout turnout l1 l2 l3 where
    SingleTurnoutLeftUp :: SingleTurnout (Turnout LeftUp n) (Link RightLink lIn) (Link LeftLink lPlus) (Link LeftLink lMinus)
    SingleTurnoutRightUp :: SingleTurnout (Turnout RightUp n) (Link LeftLink lIn) (Link RightLink lPlus) (Link RightLink lMinus)
    SingleTurnoutLeftDown :: SingleTurnout (Turnout LeftDown n) (Link RightLink lIn) (Link LeftLink lPlus) (Link LeftLink lMinus)
    SingleTurnoutRightDown :: SingleTurnout (Turnout RightDown n) (Link LeftLink lIn) (Link RightLink lPlus) (Link RightLink lMinus)

deriving instance Show (SingleTurnout turnout l1 l2 l3)

data HalfTrapezium turnout1 turnout2 l1 l2 l3 l4 where
    HalfTrapeziumUp :: HalfTrapezium (Turnout RightUp n1) (Turnout LeftDown n2) (Link LeftLink ll1) (Link LeftLink ll2) (Link RightLink lr1) (Link RightLink lr2)
    HalfTrapeziumDown :: HalfTrapezium (Turnout RightDown n1) (Turnout LeftUp n2) (Link LeftLink ll1) (Link LeftLink ll2) (Link RightLink lr1) (Link RightLink lr2)

data StationEnd l where
    StationLeftEnd :: StationEnd (Link RightLink l)
    StationRightEnd :: StationEnd (Link LeftLink l)

data Track (nr :: Nat) l1 l2 where
    Track :: Int -> Track nr (Link RightLink l1) (Link LeftLink l2)

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
        SingleTurnout turnout l1 l2 l3 -> Schema s t l -> Schema ((SingleTurnout turnout l1 l2 l3) ': s) t (l1 : l2 : l3 : l)
    TrackCons :: Track n l1 l2 -> Schema s t l -> Schema s ((Track n l1 l2) ': t) (Remove l1 (Remove l2 l))

deriving instance Show (Schema s t l)

--- ---
-- data HList :: [*] -> * where
--     HNil :: HList '[]
--     HCons :: a -> HList t -> HList (a ': t)

-- type family (s :: HList ts) :++ (x :: a) :: HList (a ': ts)
-- type family s :++ a where
--     HNil :++ x = HList [x]



u = SingleTurnoutCons (SingleTurnoutLeftUp :: SingleTurnout (Turnout LeftUp 5) (Link RightLink 1) (Link LeftLink 2) (Link LeftLink 3)) SNil
-- u = undefined
u2 = TrackCons (Track 100 :: Track 1 (Link RightLink 1) (Link LeftLink 2) ) u



