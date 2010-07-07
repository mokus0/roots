{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}
module Math.Root.Finder where

import Control.Monad.Instances ()

-- |General interface for numerical root finders.
class RootFinder r a b where
    -- |@initRootFinder f x0 x1@: Initialize a root finder for the given
    -- function with the initial bracketing interval (x0,x1).
    initRootFinder :: (a -> b) -> a -> a -> r a b
    
    -- |Step a root finder for the given function (which should generally 
    -- be the same one passed to @initRootFinder@), refining the finder's
    -- estimate of the location of a root.
    stepRootFinder :: (a -> b) -> r a b -> r a b
    
    -- |Extract the finder's current estimate of the position of a root.
    estimateRoot  :: r a b -> a
    
    -- |Extract the finder's current estimate of the upper bound of the 
    -- distance from @estimateRoot@ to an actual root in the function.
    -- 
    -- Generally, @estimateRoot r@ +- @estimateError r@ should bracket
    -- a root of the function.
    estimateError :: r a b -> a
    
    -- |Test whether a root finding algorithm has converged to a given 
    -- relative accuracy.
    converged :: (Num a, Ord a) => a -> r a b -> Bool
    converged xacc r = abs (estimateError r) <= abs xacc
    
    -- |Default number of steps after which root finding will be deemed 
    -- to have failed.
    defaultNSteps :: r a b -> Int
    defaultNSteps _ = 250

-- |@traceRoot f x0 x1 mbEps@ initializes a root finder and repeatedly
-- steps it, returning each step of the process in a list.  When the algorithm
-- terminates or the @defaultNSteps@ limit is exceeded, the list ends.
-- Termination criteria depends on @mbEps@; if it is of the form @Just eps@ 
-- then convergence to @eps@ is used (using the @converged@ method of the
-- root finder).  Otherwise, the trace is not terminated until subsequent
-- states are equal (according to '==').  This is a stricter condition than
-- convergence to 0.
traceRoot :: (Eq (r a b), RootFinder r a b, Num a, Ord a) =>
             (a -> b) -> a -> a -> Maybe a -> [r a b]
traceRoot f a b xacc = go nSteps start (stepRootFinder f start)
    where
        nSteps = defaultNSteps start
        start = initRootFinder f a b
        
        -- lookahead 1; if tracing with no convergence test, apply a
        -- naive test to bail out if the root stops changing.  This is
        -- provided because that's not always the same as convergence to 0,
        -- and the main purpose of this function is to watch what actually
        -- happens inside the root finder.
        go n x next
            | maybe (x==next) (flip converged x) xacc = [x]
            | n <= 0            = []
            | otherwise         = x : go (n-1) next (stepRootFinder f next)

-- |@findRoot f x0 x1 eps@ initializes a root finder and repeatedly
-- steps it, returning each step of the process in a list.  When the algorithm
-- converges to @eps@ or the @defaultNSteps@ limit is exceeded, the current best guess
-- is returned, with the @Right@ constructor indicating successful convergence
-- or the @Left@ constructor indicating failure to converge.
findRoot :: (RootFinder r a b, Num a, Ord a) =>
            (a -> b) -> a -> a -> a -> Either (r a b) (r a b)
findRoot f a b xacc = go nSteps start
    where
        nSteps = defaultNSteps start
        start = initRootFinder f a b
        
        go n x
            | converged xacc x  = Right x
            | n <= 0            = Left  x
            | otherwise         = go (n-1) (stepRootFinder f x)

-- |A useful constant: 'eps' is (for most 'RealFloat' types) the smallest
-- positive number such that @1 + eps /= 1@.
eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')
