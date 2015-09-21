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
module Occlusion.Core where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Function
import Data.List (minimumBy, maximumBy)
import Data.Ord  (comparing)
import Occlusion.Types
import Occlusion.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
angles :: RealFloat f => Complex f -> Polygon f -> [f]
angles = map . angle


-- |
angle :: RealFloat f => Complex f -> Complex f -> f
angle a = snd . polar . subtract a


-- |
-- TODO: Strictness, performance, move to library
minmaxBy :: (a -> a -> Ordering) -> [a] -> Maybe ((Int, a), (Int, a))
minmaxBy _ []     = Nothing
minmaxBy f (x:xs) = Just . foldr (\n (mini, maxi) -> (minimumBy (f `on` snd) [n, mini], maximumBy (f `on` snd) [n, maxi])) ((0, x), (0, x)) $ zip [0..] xs


-- |
-- TODO: Rename (?)
anglespan :: RealFloat f => Complex f -> Polygon f -> Maybe ((Int, Complex f), (Int, Complex f))
anglespan p shape = minmaxBy (comparing $ angle p) shape
