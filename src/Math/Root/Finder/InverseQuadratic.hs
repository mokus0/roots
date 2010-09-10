{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}
module Math.Root.Finder.InverseQuadratic (InverseQuadratic, inverseQuadratic) where

import Math.Root.Finder

data InverseQuadratic a b = InverseQuadratic !a !b !a !b !a !b
    deriving (Eq, Show)

-- |@inverseQuadratic f x1 x2 xacc@:  attempt to find a root of a function 
-- known to lie between x1 and x2, using the inverse quadratic interpolation 
-- method.  The root will be refined till its accuracy is +-xacc.  If
-- convergence fails, returns the final state of the search.
inverseQuadratic :: RealFloat a => (a -> a) -> a -> a -> a -> Either (InverseQuadratic a a) a
inverseQuadratic f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

instance (Fractional a, Ord a, Real b, Fractional b) => RootFinder InverseQuadratic a b where
    initRootFinder f x1 x2 = InverseQuadratic x0 (f x0) x1 (f x1) x2 (f x2)
        where x0 = 0.5 * (x1 + x2)
    stepRootFinder f orig@(InverseQuadratic x0 f0 x1 f1 x2 f2)
        | f1 /= f2  = InverseQuadratic newX newF x0 f0 x1 f1
        | otherwise = orig
        where
            newX 
                | f0 /= f1 && f0 /= f2 
                    = let a = realToFrac (f0 / (f2 - f1))
                          b = realToFrac (f1 / (f2 - f0))
                          c = realToFrac (f2 / (f1 - f0))
                       in (a * b * x2) - (a * c * x1) + (b * c * x0)
                | otherwise
                    -- Fall back to secant method (linear interpolation)
                    -- when quadratic interpolation will yield nonsensical results.
                    = x1 - realToFrac f1 * (x1 - x2) / realToFrac (f1 - f2)
            newF = f newX
    
    estimateRoot  (InverseQuadratic x0  _  _  _  _  _) = x0
    estimateError (InverseQuadratic x0  _ x1  _ x2  _) = 
        maximum [x0, x1, x2] - minimum [x0, x1, x2]
