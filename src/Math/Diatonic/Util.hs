module Math.Diatonic.Util where

import Linear

pick3Anchored :: Enum a => a -> a -> [V3 a]
pick3Anchored anchor upperRange = map (\(V2 y z) -> V3 anchor y z) $ pick2 (succ anchor) upperRange

pick1 :: Enum a => a -> a -> [V1 a]
pick1 minv maxv = [ V1 x | x <- [minv..maxv]]

pick2 :: Enum a => a -> a -> [V2 a]
pick2 minv maxv = 
    let p0 = maxv
        p1 = pred p0
    in [ (V2 x y) | x <- [minv..p1], y <- [succ x.. p0]]

pick3 :: Enum a => a -> a -> [V3 a]
pick3 minv maxv = 
    let p0 = maxv
        p1 = pred p0
        p2 = pred p1
    in [ (V3 x y z) | x <- [minv..p2], y <- [succ x..p1], z <- [succ y..p0]]

pick4 :: Enum a => a -> a -> [V4 a]
pick4 minv maxv = 
    let p0 = maxv
        p1 = pred p0
        p2 = pred p1
        p3 = pred p2
    in [ (V4 x y z w) | x <- [minv..p3], y <- [succ x..p2], z <- [succ y..p1], w <- [succ z.. p0]]

sortV2 :: Ord a => V2 a -> V2 a
sortV2 (V2 a b) = V2 (min a b) (max a b)

sortV3 :: Ord a => V3 a -> V3 a
sortV3 (V3 a b c) = 
    let
        V2 a' b' = sortV2 $ V2 a b
        V2 mina c' = sortV2 $ V2 a' c
        V2 midb maxc = sortV2 $ V2 b' c'
    in V3 mina midb maxc

sortV4 :: Ord a => V4 a -> V4 a
sortV4 (V4 a b c d) = 
    let
        V2 a' b' = sortV2 $ V2 a b
        V2 c' d' = sortV2 $ V2 c d
        V2 mina c'' = sortV2 $ V2 a' c'
        V2 b'' maxd = sortV2 $ V2 b' d'
        V2 midb midc = sortV2 $ V2 b'' c''
    in V4 mina midb midc maxd