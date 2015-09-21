-- |
-- Module      : Occlusion.Render
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 21 2015

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
module Occlusion.Render where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.IORef
import Control.Monad (forM, forM_, mapM, mapM_, when, unless)
import Control.Lens

import qualified Graphics.Rendering.Cairo as Cairo

import Occlusion.Types
import Occlusion.Lenses
import Occlusion.Vector
import qualified Occlusion.Core as Core



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
scene :: Scene -> Cairo.Render ()
scene thescene = do
  forM (thescene^.obstacles) polygon
  Cairo.setSourceRGBA 0.96 0.67 0.02 1.00
  Cairo.fill

  forM (thescene^.obstacles) $ \poly -> do
    let Just (fr, to) = Core.anglespan (thescene^.player.position) poly
    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo $ snd fr

    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo $ snd to

    Cairo.setSourceRGBA 0.28 0.71 0.84 1.00
    Cairo.setLineWidth 2
    Cairo.stroke

  shadows thescene
  character (thescene^.player)
  return ()


-- |
shadows :: Scene -> Cairo.Render ()
shadows thescene = do
  forM (thescene^.obstacles) $ \poly -> do
    shadow (thescene^.player) poly
  return ()


-- |
shadow :: Character -> Polygon Double -> Cairo.Render ()
shadow char poly = do
  let Just (fr, to) = Core.anglespan pos poly
      pos@(px:+py)  = char^.position
      [α, β] = map (snd . polar . subtract (px:+py) . snd) [fr, to]

  Cairo.resetClip

  -- Clip to shadow triangle
  vectorise Cairo.moveTo pos
  vectorise Cairo.lineTo $ pos + mkPolar 800 α
  vectorise Cairo.lineTo $ pos + mkPolar 800 β
  Cairo.clip

  -- Another clip (overlapping) encompassing the polygon and the non-occluded portion of the ground
  polygon $ [pos, snd $ fr] ++ (take (fst fr - fst to) . drop (fst to) $ cycle poly)

  Cairo.clip
  Cairo.arc px py 800 0 (2*π)
  Cairo.setFillRule Cairo.FillRuleEvenOdd

  when True $ Cairo.withRadialPattern px py 1200 px py 0 $ \pattern -> do
    Cairo.patternAddColorStopRGBA pattern 0.0 1.0 1.0 1.0 0.9
    Cairo.patternAddColorStopRGBA pattern 1.0 0.0 0.0 0.0 0.9
    Cairo.setSource pattern
    Cairo.fill

  Cairo.resetClip


-- |
polygon :: Polygon Double -> Cairo.Render ()
polygon (p:oints) = vectorise Cairo.moveTo p >> mapM (vectorise Cairo.lineTo) oints >> Cairo.closePath


-- |
character :: Character -> Cairo.Render ()
character char = do
  Cairo.arc cx cy 12.0 0 (2*π)
  Cairo.setSourceRGBA 0.47 0.04 0.37 1.00
  Cairo.fill
  where
    cx:+cy = char^.position
