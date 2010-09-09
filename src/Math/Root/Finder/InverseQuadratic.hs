{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}
module Math.Root.Finder.InverseQuadratic where

import Math.Root.Finder

data InverseQuadratic a b = InverseQuadratic !a !b !a !b !a !b
    deriving (Eq, Show)

instance (Fractional a, Ord a, Real b, Fractional b) => RootFinder InverseQuadratic a b where
    initRootFinder f x1 x2 = InverseQuadratic x0 (f x0) x1 (f x1) x2 (f x2)
        where
            x0 = 0.5 * (x1 + x2)
    stepRootFinder f (InverseQuadratic x0 f0 x1 f1 x2 f2) = InverseQuadratic newX newF x0 f0 x1 f1
        where
            newX = realToFrac (f1 * f0 / ((f2 - f1) * (f2 - f0))) * x2
                 + realToFrac (f2 * f0 / ((f1 - f2) * (f1 - f0))) * x1
                 + realToFrac (f2 * f1 / ((f0 - f2) * (f0 - f1))) * x0
            newF = f newX
    
    estimateRoot  (InverseQuadratic x0  _  _  _  _  _) = x0
    estimateError (InverseQuadratic x0  _ x1  _ x2  _) = 
        maximum [x0, x1, x2] - minimum [x0, x1, x2]
