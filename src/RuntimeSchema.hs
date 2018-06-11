{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RuntimeSchema
 ( getRuntimeSchema
 , showRS
 , allLinks
 , showTurnout
 ) where

import qualified Data.Map     as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Set     as S
import           GHC.TypeLits
import           Schema

newtype RLeftLink = RLeftLink Integer deriving (Show, Eq, Ord)
newtype RRightLink = RRightLink Integer deriving (Show, Eq, Ord)

data RuntimeSemaphore
    = RSemaphoreLeft String RRightLink RLeftLink
    | RSemaphoreRight String RLeftLink RRightLink
    deriving Show

data RuntimeTurnout
    = RTLeftUp Integer RRightLink RLeftLink RLeftLink
    | RTRightUp Integer RLeftLink RRightLink RRightLink
    | RTLeftDown Integer RRightLink RLeftLink RLeftLink
    | RTRightDown Integer RLeftLink RRightLink RRightLink
    deriving Show

data RuntimeTrack = RTrack Int Int Integer RRightLink RLeftLink deriving (Show, Eq, Ord)

data RuntimeEnd
    = RLeftEnd RRightLink
    | RRightEnd RLeftLink
    deriving Show

data RuntimeSchema = RuntimeSchema [RuntimeSemaphore] [RuntimeTurnout] [RuntimeTrack] [RuntimeEnd] deriving Show


class Runtimeable a where
    getRuntimeSchema :: a -> RuntimeSchema

instance Runtimeable (CompleteSchema (Schema s p t l u)) where
    getRuntimeSchema (CompleteSchema schema) = getRuntimeSchema' schema

class RuntimeableInternal a where
    getRuntimeSchema' :: a -> RuntimeSchema

instance RuntimeableInternal (Schema s p t l u) where
    getRuntimeSchema' = aux (RuntimeSchema [] [] [] [])
      where
        aux :: forall s p t l u. RuntimeSchema -> Schema s p t l u -> RuntimeSchema
        aux runtime SNil = runtime
        aux (RuntimeSchema ss ps ts es) (SingleTurnoutCons (SingleTurnoutLeftUp (Turnout SLeftUp :: Turnout _ n) (Link _ :: Link _ lIn) (Link _ :: Link _ lPlus) (Link _ :: Link _ lMinus) :: SingleTurnout _ _ _) rest) =
            aux (RuntimeSchema ss (RTLeftUp (natVal $ Proxy @n) (RRightLink $ natVal $ Proxy @lIn) (RLeftLink $ natVal $ Proxy @lPlus) (RLeftLink $ natVal $ Proxy @lMinus) :  ps) ts es) rest
        aux (RuntimeSchema ss ps ts es) (SingleTurnoutCons (SingleTurnoutRightUp (Turnout SRightUp :: Turnout _ n) (Link _ :: Link _ lIn) (Link _ :: Link _ lPlus) (Link _ :: Link _ lMinus) :: SingleTurnout _ _ _) rest) =
            aux (RuntimeSchema ss (RTRightUp (natVal $ Proxy @n) (RLeftLink $ natVal $ Proxy @lIn) (RRightLink $ natVal $ Proxy @lPlus) (RRightLink $ natVal $ Proxy @lMinus) :  ps) ts es) rest
        aux (RuntimeSchema ss ps ts es) (SingleTurnoutCons (SingleTurnoutLeftDown (Turnout SLeftDown :: Turnout _ n) (Link _ :: Link _ lIn) (Link _ :: Link _ lPlus) (Link _ :: Link _ lMinus) :: SingleTurnout _ _ _) rest) =
            aux (RuntimeSchema ss (RTLeftDown (natVal $ Proxy @n) (RRightLink $ natVal $ Proxy @lIn) (RLeftLink $ natVal $ Proxy @lPlus) (RLeftLink $ natVal $ Proxy @lMinus) :  ps) ts es) rest
        aux (RuntimeSchema ss ps ts es) (SingleTurnoutCons (SingleTurnoutRightDown (Turnout SRightDown :: Turnout _ n) (Link _ :: Link _ lIn) (Link _ :: Link _ lPlus) (Link _ :: Link _ lMinus) :: SingleTurnout _ _ _) rest) =
            aux (RuntimeSchema ss (RTRightDown (natVal $ Proxy @n) (RLeftLink $ natVal $ Proxy @lIn) (RRightLink $ natVal $ Proxy @lPlus) (RRightLink $ natVal $ Proxy @lMinus) :  ps) ts es) rest

        aux (RuntimeSchema ss ps ts es) (TrackCons (Track level len (Link SRightLink :: Link _ l1) (Link SLeftLink :: Link _ l2) :: Track n _ _) rest) =
            aux (RuntimeSchema ss ps (RTrack level len (natVal $ Proxy @n) (RRightLink $ natVal $ Proxy @l1) (RLeftLink $ natVal $ Proxy @l2) : ts) es) rest

        aux (RuntimeSchema ss ps ts es) (StationEndCons (StationLeftEnd (Link _ :: Link _ sel)) rest) =
            aux (RuntimeSchema ss ps ts ((RLeftEnd $ RRightLink $ natVal $ Proxy @sel) : es)) rest
        aux (RuntimeSchema ss ps ts es) (StationEndCons (StationRightEnd (Link _ :: Link _ sel)) rest) =
            aux (RuntimeSchema ss ps ts ((RRightEnd $ RLeftLink $ natVal $ Proxy @sel) : es)) rest

        aux (RuntimeSchema ss ps ts es) (SemaphoreCons (SemaphoreLeft (Link SRightLink :: Link _ l1) (Link SLeftLink :: Link _ l2) :: Semaphore name _ _) rest) =
            aux (RuntimeSchema (RSemaphoreLeft (symbolVal $ Proxy @name) (RRightLink $ natVal $ Proxy @l1) (RLeftLink $ natVal $ Proxy @l2) : ss) ps ts es) rest
        aux (RuntimeSchema ss ps ts es) (SemaphoreCons (SemaphoreRight (Link SLeftLink :: Link _ l1) (Link SRightLink :: Link _ l2) :: Semaphore name _ _) rest) =
            aux (RuntimeSchema (RSemaphoreRight (symbolVal $ Proxy @name) (RLeftLink $ natVal $ Proxy @l1) (RRightLink $ natVal $ Proxy @l2) : ss) ps ts es) rest
-- todo


allLinks :: RuntimeSchema -> ([RLeftLink], [RRightLink])
allLinks = aux ([], [])
  where
    aux (rlls, rrls) (RuntimeSchema [] [] [] []) = (rlls, rrls)
    aux (rlls, rrls) (RuntimeSchema (RSemaphoreLeft _ r l : ss) ps ts es) = aux (l : rlls, r : rrls) (RuntimeSchema ss ps ts es)
    aux (rlls, rrls) (RuntimeSchema (RSemaphoreRight _ l r : ss) ps ts es) = aux (l : rlls, r :rrls) (RuntimeSchema ss ps ts es)
    aux (rlls, rrls) (RuntimeSchema [] (p:ps) ts es) = aux (let (nl, nr) = turnoutLinks p in (nl ++ rlls, nr ++ rrls)) (RuntimeSchema [] ps ts es)
    aux (rlls, rrls) (RuntimeSchema [] [] (RTrack _ _ _ r l : ts) es) = aux (l : rlls, r : rrls) (RuntimeSchema [] [] ts es)
    aux (rlls, rrls) (RuntimeSchema [] [] [] (e:es)) = aux (case e of
        RLeftEnd r  -> (rlls, r : rrls)
        RRightEnd l -> (l : rlls, rrls)) (RuntimeSchema [] [] [] es)

turnoutLinks :: RuntimeTurnout -> ([RLeftLink], [RRightLink])
turnoutLinks (RTLeftUp _ r l1 l2)    = ([l1, l2], [r])
turnoutLinks (RTLeftDown _ r l1 l2)  = ([l1, l2], [r])
turnoutLinks (RTRightUp _ l r1 r2)   = ([l], [r1, r2])
turnoutLinks (RTRightDown _ l r1 r2) = ([l], [r1, r2])

showRS :: RuntimeSchema -> String
showRS runtime =
    show positionedTurnouts ++ "\n\n" ++ unlines (printObjs $ trackObjs ++ turnoutObjs ++ semaphoreObjs)
  where
    RuntimeSchema semaphores turnouts tracks ends = runtime
    (lTrackMap, rTrackMap) = foldr getTrack (M.empty, M.empty) tracks
      where
        getTrack t@(RTrack _ _ _ r l) (lMap, rMap) = (M.insert l t lMap, M.insert r t rMap)
    edges :: M.Map RuntimeTrack [(RuntimeTrack, Int)]
    edges = foldr getTurnout M.empty turnouts
      where
        getTurnout (RTLeftUp _ r lPlus lMinus) mp    = addEdge (rTrackMap M.! r) (lTrackMap M.! lPlus, 0)
                                                     $ addEdge (lTrackMap M.! lPlus) (rTrackMap M.! r, 0)
                                                     $ addEdge (rTrackMap M.! r) (lTrackMap M.! lMinus, 1) mp
        getTurnout (RTLeftDown _ r lPlus lMinus) mp  = addEdge (rTrackMap M.! r) (lTrackMap M.! lPlus, 0)
                                                     $ addEdge (lTrackMap M.! lPlus) (rTrackMap M.! r, 0)
                                                     $ addEdge (lTrackMap M.! lMinus) (rTrackMap M.! r, 1) mp
        getTurnout (RTRightUp _ l rPlus rMinus) mp   = addEdge (lTrackMap M.! l) (rTrackMap M.! rPlus, 0)
                                                     $ addEdge (rTrackMap M.! rPlus) (lTrackMap M.! l, 0)
                                                     $ addEdge (lTrackMap M.! l) (rTrackMap M.! rMinus, 1) mp
        getTurnout (RTRightDown _ l rPlus rMinus) mp = addEdge (lTrackMap M.! l) (rTrackMap M.! rPlus, 0)
                                                     $ addEdge (rTrackMap M.! rPlus) (lTrackMap M.! l, 0)
                                                     $ addEdge (rTrackMap M.! rMinus) (lTrackMap M.! l, 1) mp

    addEdge :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
    addEdge k v mp = case k `M.lookup` mp of
        Just oldV -> M.insert k (v : oldV) mp
        Nothing   -> M.insert k [v] mp


    (trackLinkDeps, lLevels, rLevels) = foldr processTrack (M.empty, M.empty, M.empty) tracks
      where
        processTrack (RTrack level len _ r l) (linkDeps, lLevels, rLevels) = (M.insert l (r, len) linkDeps, M.insert l level lLevels, M.insert r level rLevels)

    lrDeps :: M.Map RRightLink [(RLeftLink, Int)]
    lrDeps = foldr processTurnout M.empty turnouts `M.union` foldr processSemaphores M.empty semaphores
      where
        processTurnout (RTLeftUp _ r lPlus lMinus) mp = M.insert r [getEdge r lPlus, getEdge r lMinus] mp
        processTurnout (RTLeftDown _ r lPlus lMinus) mp = M.insert r [getEdge r lPlus, getEdge r lMinus] mp
        processTurnout (RTRightUp _ l rPlus rMinus) mp = M.insert rPlus [getEdge rPlus l] $ M.insert rMinus [getEdge rMinus l] mp
        processTurnout (RTRightDown _ l rPlus rMinus) mp = M.insert rPlus [getEdge rPlus l] $ M.insert rMinus [getEdge rMinus l] mp
        getEdge r l = let levelDiff = 1 `max` abs (rLevels M.! r - lLevels M.! l) in (l, 3 * (levelDiff + 2))

        processSemaphores :: RuntimeSemaphore -> M.Map RRightLink [(RLeftLink, Int)] -> M.Map RRightLink [(RLeftLink, Int)]
        processSemaphores (RSemaphoreLeft _ r l)  = M.insert r [(l, 2)]
        processSemaphores (RSemaphoreRight _ l r) = M.insert r [(l, 2)]


    startingRs :: M.Map RRightLink Int
    startingRs = M.fromList $ mapMaybe aux ends
      where
        aux (RLeftEnd r)  = Just (r, 0)
        aux (RRightEnd _) = Nothing

    absoluteL :: M.Map RLeftLink Int
    absoluteR :: M.Map RRightLink Int
    (absoluteL, absoluteR) = process (M.empty, startingRs) (M.keysSet lLevels, M.keysSet rLevels S.\\ M.keysSet startingRs)
      where
        process :: (M.Map RLeftLink Int, M.Map RRightLink Int) -> (S.Set RLeftLink, S.Set RRightLink) -> (M.Map RLeftLink Int, M.Map RRightLink Int)
        process (absL, absR) (unknownL, unknownR) = case (S.null unknownL, S.null unknownR) of
            (True, True) -> (absL, absR)
            _ -> process (newAbsL, newAbsR) (unknownL S.\\ M.keysSet newAbsL, unknownR S.\\ M.keysSet newAbsR)
          where
            newAbsL = absL `M.union` M.fromList (mapMaybe aux $ S.toList $ S.map (\l -> let (r, len) = trackLinkDeps M.! l in (l, len, r `M.lookup` absR)) unknownL)
            aux (l, len, Just start) = Just (l, start + len)
            aux (_, _, Nothing)      = Nothing

            newAbsR = absR `M.union` M.fromList (mapMaybe (newRAux . (\r -> (r, map lrDepAux $ lrDeps M.! r))) $ S.toList unknownR)
            lrDepAux (l, len) = case l `M.lookup` newAbsL of
                Just start -> Just (start + len)
                Nothing    -> Nothing
            newRAux (r, deps) = if all isJust deps
                then Just (r, maximum $ catMaybes deps)
                else Nothing

    turnoutSize :: RuntimeTurnout -> Int
    turnoutSize t = case t of
        RTLeftUp _ r _ l    -> tsize r l
        RTLeftDown _ r _ l  -> tsize r l
        RTRightUp _ l _ r   -> tsize r l
        RTRightDown _ l _ r -> tsize r l
      where
        tsize r l = 1 `max` abs (rLevels M.! r - lLevels M.! l)

    positionedTurnouts :: [(RuntimeTurnout, (Int, Int))]
    positionedTurnouts = map aux turnouts
      where
        aux t@(RTLeftUp _ r lPlus lMinus)       = (t, (3 * (lLevels M.! lMinus), absoluteR M.! r - width t))
        aux t@(RTLeftDown _ r lPlus lMinus)     = (t, (3 * (lLevels M.! lPlus), absoluteR M.! r - width t))
        aux t@(RTRightUp _ l rPlus rMinus)      = (t, (3 * (rLevels M.! rMinus), absoluteR M.! rMinus - width t))
        aux t@(RTRightDown _ l rPlus rMinus)    = (t, (3 * (rLevels M.! rPlus), absoluteR M.! rMinus - width t))
        width t = 3 * (turnoutSize t + 2)

    turnoutObjs :: [((Int, Int), [String])]
    turnoutObjs = map (\(t, position) -> (position, getStrings t)) positionedTurnouts
      where
        getStrings :: RuntimeTurnout -> [String]
        getStrings t@(RTLeftUp nr _ _ _)    = showTurnout SLeftUp (turnoutSize t) nr Plus
        getStrings t@(RTRightUp nr _ _ _)   = showTurnout SRightUp (turnoutSize t) nr Plus
        getStrings t@(RTLeftDown nr _ _ _)  = showTurnout SLeftDown (turnoutSize t) nr Plus
        getStrings t@(RTRightDown nr _ _ _) = showTurnout SRightDown (turnoutSize t) nr Plus

    trackObjs :: [((Int, Int), [String])]
    trackObjs = map aux tracks
      where
        aux t@(RTrack level len nr r l) = ((3 * level, absoluteR M.! r), showTrack t)

    semaphoreObjs :: [((Int, Int), [String])]
    semaphoreObjs = map aux semaphores
      where
        aux (RSemaphoreLeft name r l) = ((3 * (rLevels M.! r) - 1, absoluteR M.! r - 2), [name, "◀<"])
        aux (RSemaphoreRight name l r) = ((3 * (rLevels M.! r), absoluteR M.! r - 2), [">▶", " " ++ name])

printObjs :: [((Int, Int), [String])] -> [String]
printObjs objs = fromCharMap $ foldr draw (toCharMap canvas) objs
  where
    toCharMap :: [String] -> M.Map Int (M.Map Int Char)
    toCharMap ss = M.map (M.fromList . zip [1..]) $ M.fromList (zip [1..] ss)

    fromCharMap :: M.Map Int (M.Map Int Char) -> [String]
    fromCharMap mp = map M.elems $ M.elems mp

    canvas = replicate height $ replicate width ' '
    height = maximum $ map (\((x, _), ss) -> x + length ss) objs
    width = maximum $ map (\((_, y), ss) -> y + maximum (map length ss)) objs

    draw ((x, y), ss) accCanvas = M.foldrWithKey auxLine accCanvas (M.mapKeys (+x) $ M.map (M.mapKeys (+y)) (toCharMap ss))
    auxLine :: Int -> M.Map Int Char -> M.Map Int (M.Map Int Char) -> M.Map Int (M.Map Int Char)
    auxLine line mp accCanvas = M.insert line (M.foldrWithKey auxChar (accCanvas M.! line) mp) accCanvas
    auxChar nr char = if char /= '@' then M.insert nr char else id


showTurnout :: STurnoutDirection td -> Int -> Integer -> TurnoutState -> [String]
showTurnout direction size nr state = case (direction, state) of
    (SLeftUp, Plus) ->
        cap direction ++ concatMap (line direction) [1..size - 1] ++
        [ pad (size-1) ++ "    ╲ " ++ nrRep ++ spaces
        , pad (size-1) ++ "─────────"
        ]
    (SLeftUp, Minus) ->
        cap direction ++ concatMap (line direction) [1..size - 1] ++
        [ pad (size-1) ++ "    ╲ " ++ nrRep ++ spaces
        , pad (size-1) ++ "───  ╲───"
        ]
    (SRightUp, Plus) ->
        cap direction ++ concatMap (line direction) [1..size - 1] ++
        [ spaces ++ nrRep ++ " ╱    "
        , "─────────"
        ]
    (SRightUp, Minus) ->
        cap direction ++ concatMap (line direction) [1..size - 1] ++
        [ spaces ++ nrRep ++ " ╱    "
        , "───╱  ───"
        ]
    (SLeftDown, Plus) ->
        [ pad (size-1) ++ "─────────"
        , pad (size-1) ++ "    ╱ " ++ nrRep ++ spaces
        ] ++ concatMap (line direction) [1..size - 1] ++ cap direction
    (SLeftDown, Minus) ->
        [ pad (size-1) ++ "───  ╱───"
        , pad (size-1) ++ "    ╱ " ++ nrRep ++ spaces
        ] ++ concatMap (line direction) [1..size - 1] ++ cap direction
    (SRightDown, Plus) ->
        [ "─────────"
        , spaces ++ nrRep ++ " ╲    "
        ] ++ concatMap (line direction) [1..size - 1] ++ cap direction
    (SRightDown, Minus) ->
        [ "───╲  ───"
        , spaces ++ nrRep ++ " ╲    "
        ] ++ concatMap (line direction) [1..size - 1] ++ cap direction
  where
    nrRep = show nr
    spaces = replicate (3 - length nrRep) ' '
    cap :: STurnoutDirection td -> [String]
    cap SLeftUp    = map (++ pad size) ["──╲   ", "   ╲  "]
    cap SRightUp   = map (pad size ++) ["   ╱──", "  ╱   "]
    cap SLeftDown  = map (++ pad size) ["   ╱  ", "──╱   "]
    cap SRightDown = map (pad size ++) ["  ╲   ", "   ╲──"]

    line :: STurnoutDirection td -> Int -> [String]
    line SLeftUp n      = map ((pad n ++) . (++ pad (size - n))) [" ╲    ", "  ╲   ", "   ╲  "]
    line SRightUp n     = map ((pad (size - n) ++) . (++ pad n)) ["    ╱ ", "   ╱  ", "  ╱   "]
    line SLeftDown n    = map ((pad (size - n) ++) . (++ pad n)) ["   ╱  ", "  ╱   ", " ╱    "]
    line SRightDown n   = map ((pad n ++) . (++ pad (size - n))) ["  ╲   ", "   ╲  ", "    ╲ "]

    pad n = replicate (n * 3) '@'


showTrack :: RuntimeTrack -> [String]
showTrack (RTrack _ len nr _ _) = [replicate leftLen '─' ++ nrRep ++ replicate rightLen '─']
  where
    nrRep = if nr > 0 then show nr else ""
    leftLen = (len - length nrRep) `div` 2
    rightLen = len - leftLen - length nrRep
