{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Root.Finder.Brent
    ( Brent
    , brent
    ) where

import Math.Root.Bracket
import Math.Root.Finder
import Data.Maybe
import Text.Printf

-- Invariants:
--  1)  B and C bracket the root
--  2)  |f(B)| <= |f(C)|
--  3)  min(f(B),f(C)) <= f(A) <= max(f(B),f(C))
-- |Working state for Brent's root-finding method.
data Brent a b = Brent
    { brA   :: !a
    , brFA  :: !b
    , brB   :: !a
    , brFB  :: !b
    , brC   :: !a
    , brFC  :: !b
    , brE   :: a
    } deriving (Eq, Show)

-- TODO: clean up this mess!
instance RealFloat a => RootFinder Brent a a where
    initRootFinder f x1 x2 = fixMagnitudes (Brent x1 f1 x2 f2 x1 f1 dx)
        where f1 = f x1; f2 = f x2; dx = x2 - x1
    
    stepRootFinder f r@(Brent a fa b fb c fc e)
        |  abs e                >= 2 * min tol1 abs_s       -- require that the method be making progress, overall
        && 1.5 * m * signum s   >= tol1 + abs_s             -- require that the proposed step is getting closer to 'b' - specifically, s should be between 0 and 0.75*(c - b)
                    = advance s s
        | otherwise = advance m (b - a)
        where
            -- Minimum step size to continue with inverse-quadratic interpolation
            tol1  = eps * (abs b + 0.5)
            abs_s = abs s
            
            -- midpoint for bisection step
            m = 0.5 * (c - b)
            
            -- subdivision point for inverse quadratic interpolation step
            s   | fa /= fc && fa /= fb
                    = let a' = fa / (fc - fb)
                          b' = fb / (fc - fa)
                          c' = fc / (fb - fa)
                       in (a' * b' * c) - ((a' * c' + 1) * b) + (a * b' * c')
                | otherwise
                    -- Fall back to linear interpolation when quadratic
                    -- interpolation will yield nonsensical results.
                    = fb * (c - b) / realToFrac (fb - fc)
            
            -- |Moves the current estimate by 'd' (or by tol1, whichever
            -- is greater) and sets 'brE' to 'e', maintaining all invariants.
            -- Ensuring that at least some tiny jump is made allows quick 
            -- discovery and termination in the case where the current best
            -- estimate is already nearly on top of the root.  Without such
            -- a check, the method would repeatedly tighten the 'c' bound
            -- by bisection every other step, which is really rather stupid
            -- if 'b' is already sitting on a root.
            advance d e = update b' (f b') e r
                where
                    b' = if abs d > tol1 then b + d else b + tol1 * signum m


    estimateRoot  = brB
    estimateError = brE
    converged   _ Brent{brFB = 0}   = True
    converged tol br@Brent{brB = b, brE = e} = 
        abs e <= 4 * eps * abs b + tol

-- |Attempt to find a root of a function known to lie between x1 and x2, using 
-- Brent's method.  The root will be refined till its accuracy is +-xacc.  
-- If convergence fails, returns the final state of the search.
brent :: RealFloat a => (a -> a) -> a -> a -> a -> Either (Brent a a) a
brent f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

-- |Updates the state by incorporating a new estimate and setting 'brE',
-- maintaining all invariants.
update :: (Num a, Num b, Ord b) => a -> b -> a -> Brent a b -> Brent a b
update b fb e r@Brent{brB = a, brFB = fa} 
    = fixMagnitudes (fixSigns r{brA = a, brFA = fa, brB = b, brFB = fb, brE = e})

-- Establish invariant (1) that b and c bracket the root,
-- based on precondition that (a,c) already does.
-- 
-- (a,c) brackets implies that either (b,c) or (a,b) brackets.  In the 
-- former case, nothing needs to be done as (by construction) either fb is already
-- between fa and fc or b is already between a and c (depending which kind of 
-- step was taken).  In the latter case, discard C and use A in its place, because
-- c and fc are both (by the existing invariants - (a,c) bracket, |f(c)| >= |f(a)|) 
-- outside the new region of interest.
fixSigns :: (Num a, Num b, Ord b) => Brent a b -> Brent a b
fixSigns br@Brent{ brA  =  a, brB  =  b
                 , brFA = fa, brFB = fb, brFC = fc }
    |  (fb > 0 && fc > 0) || (fb < 0 && fc < 0)
    = br { brC = a, brFC = fa }
    | otherwise 
    = br

-- Establish invariant (2) that |f(c)| >= |f(b)| and invariant (3) that
-- 'fa' falls between fb and fc.
fixMagnitudes :: (Num b, Ord b) => Brent a b -> Brent a b
fixMagnitudes br@Brent{ brC  =  c, brB  =  b
                      , brFC = fc, brFB = fb }
    | abs fc < abs fb
    = br { brA = b, brFA = fb
         , brB = c, brFB = fc
         , brC = b, brFC = fb
         }
    | otherwise 
    = br

-- |debugging function to show a nice trace of the progress of the algorithm
_traceBrent :: (PrintfArg a, RealFloat a,
                PrintfArg b, Ord b, Num b,
                RootFinder Brent a b) =>
               (a -> b) -> Maybe (a, a) -> IO ()
_traceBrent f mbRange = do
    xs <- sequence
        [ put br >> return br
        | br <- traceRoot f x0 x1 (Just eps)
        ]

    putStrLn "(converged)"
    go (last xs)
    where 
        (x0,x1) = fromMaybe (last (bracket f 0 1)) mbRange
        put Brent{brA=a, brB=b, brC=c, brFA=fa, brFB=fb, brFC=fc} = 
            putStrLn . map fixPlus $
            printf (concat (replicate 6 "%-+25g")) a b c fa fb fc
        fixPlus '+' = ' '
        fixPlus c = c
        go x 
            | x == x'   = return ()
            | otherwise = put x >> go x'
            where x' = stepRootFinder f x