-- |
-- Module      : Occlusion.Core
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 20 2015

-- TODO | - Angles utilities, units, normalise angles
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
-- {-# LANGUAGE ScopedTypeVariables #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Occlusion.Core where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Function
import Data.Fixed
import Data.List (minimumBy, maximumBy)
import Data.Ord  (comparing)

import Occlusion.Types
import Occlusion.Lenses
import Occlusion.Vector



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
angles :: RealFloat f => Complex f -> Polygon f -> [f]
angles = map . angle


-- |
angle :: RealFloat f => Complex f -> Complex f -> f
angle a = snd . polar . subtract a


-- | Finds the minimum and maximum value in a list, by some arbitrary criterion.
-- TODO: Strictness, performance, move to library
minmaxBy :: (a -> a -> Ordering) -> [a] -> Maybe ((Int, a), (Int, a))
minmaxBy _ []     = Nothing
minmaxBy f (x:xs) = Just . foldr (\n (mini, maxi) -> (minBy f n mini, maxBy f n maxi)) ((0, x), (0, x)) $ zip [1..] xs
  where
    minBy f n mini = maximumBy (f `on` snd) [n, mini]
    maxBy f n maxi = minimumBy (f `on` snd) [n, maxi]


-- |
-- TODO: Rename (?)
-- TODO: Maybe it would be a good idea if a function called 'anglespan' actually returned some angles.
-- TODO: Think radar
anglespan :: RealFloat f => Complex f -> Polygon f -> Maybe ((Int, Complex f), (Int, Complex f))
anglespan p shape = minmaxBy (comparing $ normalise . angle p) shape


-- |
normalise :: RealFloat f => f -> f
normalise = flip mod' $ 2*π


-- |
-- TODO: Rename (eg. occlusionEdge, frontEdge, near/far, cover, etc.) (?)
nearestEdge :: RealFloat f => Complex f -> Polygon f -> Edge f
nearestEdge p poly = error ""


-- |
-- TODO: Rename (eg. distant, etc.) (?)
-- TODO: Rigorous algorithm
-- TODO: I could solve this with intersect testing...
-- TODO: Or comparing (ai < bi) to (α < β)
distantEdge :: (RealFloat f, Ord f) => Complex f -> Polygon f -> Maybe (Edge f)
distantEdge p shape = case span' of
  -- Just ((ai, α), (bi, β)) -> Just $ slice (min ai bi) (max ai bi) shape -- TODO: This line needs some love and attention
  -- Just ((ai, α), (bi, β)) -> Just $ slice (max ai bi) (max ai bi+length shape - min ai bi) $ cycle shape -- TODO: This line needs some love and attention
  Just ((ai, a), (bi, b)) -> Just $ slice (max ai bi) (length shape + max ai bi - 1) $ cycle shape
  -- Just ((ai, fr), (bi, to)) -> Just $ if (ai < bi) == (normalise (angle p fr) < normalise (angle p to))
  --                                       then slice (max ai bi) (min ai bi + length shape) (cycle shape)
  --                                       else slice (min ai bi) (max ai bi) (shape)
  Nothing                 -> Nothing
  where
    span' = anglespan p shape
    -- gap   =
    -- from = _
    -- to   = _


-- |
-- TODO: Cyclic slice
slice :: Int -> Int -> [a] -> [a]
slice fr to = take (to-fr) . drop fr
