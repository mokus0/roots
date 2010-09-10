{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Math.Root.Finder.Dekker (Dekker) where

import Math.Root.Finder

-- fields: a fa b fb oldB oldFb
-- invariants: 
--  1) signum fA /= signum fB
--  2) abs fB <= abs fA
--  3) (oldB-a)*(oldB-b) >= 0
data Dekker a b = Dekker !a !b !a !b !a !b  deriving (Eq, Show)

instance (Fractional a, Ord a, Real b, Fractional b, Ord b) => RootFinder Dekker a b where
    initRootFinder f x0 x1 
        | signum f0 == signum f1    = error "initRootFinder/Dekker: starting points do not (obviously) bracket a root"
        | abs f0 <= abs f1          = Dekker x1 f1 x0 f0 x1 f1
        | otherwise                 = Dekker x0 f0 x1 f1 x0 f0
        where f0 = f x0; f1 = f x1
    
    stepRootFinder f orig@(Dekker a _ b fb oldB oldFb)
        | fb == 0               = orig
        |  oldFb /= fb 
        && s `between` (a,b)    = step s (f s) orig
        | otherwise             = step m (f m) orig
        where
            s = b - (b * oldB) * realToFrac (fb / (fb - oldFb))
            m = 0.5 * (a + b)
    
    estimateRoot  (Dekker _ _ b _ _ _) = b
    estimateError (Dekker a _ b _ _ _) = a - b

between :: Ord a => a -> (a,a) -> Bool
a `between` (x,y) = (a > min x y) && (a < max x y)

-- |Incorporates a new point, maintaining invariant 1, assuming invariant 3,
-- and using 'accept' to restore invariant 2.
step :: (Num b, Ord b) => a -> b -> Dekker a b -> Dekker a b
step x fx orig@(Dekker a fa b fb _ _)
    | signum fx /= signum fa    = accept a fa x fx orig
    | otherwise                 = accept x fx b fb orig

-- |Re-establishes invariant 2 (abs fb <= abs fa) without affecting invariants 1 and 3.
accept :: (Num b, Ord b) => a -> b -> a -> b -> Dekker a b -> Dekker a b
accept a fa b fb (Dekker _ _ oldB oldFb _ _)
    | abs fb <= abs fa          = Dekker a fa b fb oldB oldFb
    | otherwise                 = Dekker b fb a fa oldB oldFb