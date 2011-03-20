{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}
module Math.Root.Finder
    ( RootFinder(..)
    , getDefaultNSteps
    , runRootFinder
    , traceRoot
    , findRoot, findRootN
    , eps
    ) where

import Control.Monad.Instances ()
import Data.Tagged

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
    -- to have failed.  Purely a convenience used to control the behavior
    -- of built-in functions such as 'findRoot' and 'traceRoot'.  The
    -- default value is 250.
    defaultNSteps :: Tagged (r a b) Int
    defaultNSteps = Tagged 250

-- |Convenience function to access 'defaultNSteps' for a root finder, 
-- which requires a little bit of type-gymnastics.
-- 
-- This function does not evaluate its argument.
getDefaultNSteps :: RootFinder r a b => r a b -> Int
getDefaultNSteps rf = nSteps
    where
        Tagged nSteps = 
            (const :: Tagged a b -> a -> Tagged a b)
            defaultNSteps rf

-- |General-purpose driver for stepping a root finder.  Given a \"control\"
-- function, the function being searched, and an initial 'RootFinder' state,
-- @runRootFinder step f state@ repeatedly steps the root-finder and passes
-- each intermediate state, along with a count of steps taken, to @step@.
-- 
-- The @step@ funtion will be called with the following arguments:
--
-- [@ n :: 'Int' @] 
--  The number of steps taken thus far
-- 
-- [@ currentState :: r a b @]
--  The current state of the root finder
--
-- [@ continue :: c @]
--  The result of the \"rest\" of the iteration
--
-- For example, the following function simply iterates a root finder
-- and returns every intermediate state (similar to 'traceRoot'):
-- 
-- > iterateRoot :: RootFinder r a b => (a -> b) -> a -> a -> [r a b]
-- > iterateRoot f a b = runRootFinder (const (:)) f (initRootFinder f a b)
--
-- And the following function simply iterates the root finder to 
-- convergence or throws an error after a given number of steps:
--
-- > solve :: (RootFinder r a b, RealFloat a)
-- >       => Int -> (a -> b) -> a -> a -> r a b
-- > solve maxN f a b = runRootFinder step f (initRootFinder f a b)
-- >    where
-- >        step n x continue
-- >            | converged eps x   = x
-- >            | n > maxN          = error "solve: step limit exceeded"
-- >            | otherwise         = continue
-- 
runRootFinder :: (RootFinder r a b) =>
    (Int -> r a b -> c -> c) -> (a -> b) -> r a b -> c
runRootFinder cons f = go 0
    where
        go n x = n `seq` cons n x (go (n+1) (stepRootFinder f x))

-- |@traceRoot f x0 x1 mbEps@ initializes a root finder and repeatedly
-- steps it, returning each step of the process in a list.  No step limit
-- is imposed.
-- 
-- Termination criteria depends on @mbEps@; if it is of the form @Just eps@ 
-- then convergence to @eps@ is used (using the @converged@ method of the
-- root finder).  Otherwise, the trace is not terminated until subsequent
-- states are equal (according to '==').  This is a stricter condition than
-- convergence to 0; subsequent states may have converged to zero but as long
-- as any internal state changes the trace will continue.
traceRoot :: (Eq (r a b), RootFinder r a b, Num a, Ord a) =>
             (a -> b) -> a -> a -> Maybe a -> [r a b]
traceRoot f a b mbEps = runRootFinder cons f start
    where
        start = initRootFinder f a b
        
        cons _n x rest
            | done x rest   = [x]
            | otherwise     = x : rest
        
        -- if tracing with no convergence test, apply a naive test
        -- to bail out if the root stops changing.  This is provided 
        -- because that's not always the same as convergence to 0,
        -- and the main purpose of this function is to watch what 
        -- actually happens inside the root finder.
        done = case mbEps of
            Nothing     -> \x (next:_)  -> x == next
            Just xacc   -> \x _rest     -> converged xacc x

-- |@findRoot f x0 x1 eps@ initializes a root finder and repeatedly
-- steps it.  When the algorithm converges to @eps@ or the 'defaultNSteps'
-- limit is exceeded, the current best guess is returned, with the @Right@ 
-- constructor indicating successful convergence or the @Left@ constructor 
-- indicating failure to converge.
findRoot :: (RootFinder r a b, Num a, Ord a) =>
            (a -> b) -> a -> a -> a -> Either (r a b) (r a b)
findRoot f a b xacc = result
    where
        result = findRootN nSteps f a b xacc
        nSteps = getDefaultNSteps (either id id result)

-- |Like 'findRoot' but with a specified limit on the number of steps (rather
-- than using 'defaultNSteps').
findRootN :: (RootFinder r a b, Num a, Ord a) =>
            Int -> (a -> b) -> a -> a -> a -> Either (r a b) (r a b)
findRootN nSteps f a b xacc = runRootFinder step f start
    where
        start = initRootFinder f a b
        
        step n x continue
            | converged xacc x  = Right x
            | n > nSteps        = Left  x
            | otherwise         = continue

-- |A useful constant: 'eps' is (for most 'RealFloat' types) the smallest
-- positive number such that @1 + eps /= 1@.
eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

