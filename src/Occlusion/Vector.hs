-- |
-- Module      : Occlusion.Vector
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created Jonatan H Sundqvist 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Occlusion.Vector where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor
import Control.Applicative
import Control.Lens

import Occlusion.Types
import Occlusion.Lenses


--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
π :: RealFloat r => r
π = pi



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
vectorise :: (a -> a -> b) -> Complex a -> b
vectorise f (x:+y) = f x y

-- Plumbing --------------------------------------------------------------------------------------------------------------------------------

-- |
dotwise :: (a -> a -> a) -> Complex a -> Complex a -> Complex a
dotwise f (x:+y) (x':+y') = f x x':+f y y'


-- |
dotmap :: (a -> a) -> Complex a -> Complex a
dotmap f (x:+y) = f x:+f y

-- Linear functions ------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Refactor
-- TODO: Invariants, check corner cases
-- TODO: Deal with vertical lines
-- TODO: Factor out infinite-line logic
-- TODO: Decide how to deal with identical lines
-- TODO: Factor out domain logic (eg. write restrict or domain function)
-- TODO: Visual debugging functions
intersect :: RealFloat f => Line f -> Line f -> Maybe (Complex f)
-- intersect f@(Line a b) g@(Line a' b')
intersect f g
  | slope f == slope g = Nothing -- TODO: Handle this case explicitly (eg. with linear) (?)
  -- | -- TODO: Deal with slope == Infinity
  | otherwise = (,) <$> linear f <*> linear g >>= uncurry linearIntersect -- TODO: Refactor (?)


-- | Gives the linear function overlapping the given segment
linear :: RealFloat f => Line f -> Maybe (Linear f)
linear line = (,) <$> slope line <*> intercept line


-- | Finds the intersection (if any) of two linear functions
linearIntersect :: RealFloat f => Linear f -> Linear f -> Maybe (Complex f)
linearIntersect (kf, mf) (kg, mg)
  | kf == kg  = Nothing
  | otherwise = let x = (mg-mf)/(kg-kg)
                    y = (mf*x + kf)
                in Just $ x:+y


-- |
slope :: RealFloat f => Line f -> Maybe f
slope (Line fr to)
  | dx == 0   = Nothing
  | otherwise = Just $ dy/dx
  where
    (dx:+dy) = to - fr


-- |
intercept :: Line f -> Maybe f
intercept (Line a b) = error ""

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
between :: Ord a => a -> a -> a -> Bool
between mini maxi a = mini <= a && a <= maxi


-- | Ensures that a given point lies within the domain and codomain
-- TODO: Let thus function work on scalars, write another function for domain and codomain (?)
-- restrict domain codomain p = _
restrict :: Ord f => Complex f -> Complex f -> Complex f -> Maybe (Complex f)
restrict a b p@(x:+y)
  | indomain && incodomain = Just p
  | otherwise              = Nothing
  where
    (lowx:+lowy)   = dotwise min a b
    (highx:+highy) = dotwise max a b
    indomain       = between lowx highx x
    incodomain     = between lowy highy y
