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
  -- Render obstacles
  forM (thescene^.obstacles) $ \poly -> do
    polygon poly
    Cairo.setSourceRGBA 0.96 0.67 0.02 1.00
    Cairo.fill

  -- Render span lines
  forM (thescene^.obstacles) $ \poly -> do
    let Just (fr, to) = Core.anglespan (thescene^.player.position) poly

    Cairo.setSourceRGBA 0.28 0.71 0.84 1.00
    Cairo.setLineWidth 2

    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo $ snd fr
    Cairo.stroke

    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo $ snd to
    Cairo.stroke

  -- Render shadows
  shadows thescene

  -- Render player
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

  -- Cairo.resetClip
  -- Cairo.setFillRule Cairo.FillRuleEvenOdd

  -- Clip to shadow triangle
  -- vectorise Cairo.moveTo pos
  -- vectorise Cairo.lineTo $ pos + mkPolar 800 α
  -- vectorise Cairo.lineTo $ pos + mkPolar 800 β
  -- Cairo.clip
  -- Cairo.resetClip

  -- Another clip (overlapping) encompassing the polygon and the non-occluded portion of the ground
  -- polygon $ [pos, snd $ fr] ++ (take (fst fr - fst to) . drop (fst to) $ cycle poly)
  -- Cairo.clip
  Cairo.moveTo px py
  arc 1200 (min α β) (max α β) pos
  Cairo.setSourceRGBA 0.31 0.31 0.31 0.47
  Cairo.fill
  -- Cairo.clip

  when False $ Cairo.withRadialPattern px py 1200 px py 0 $ \pattern -> do
    Cairo.patternAddColorStopRGBA pattern 0.0 1.0 1.0 1.0 0.9
    Cairo.patternAddColorStopRGBA pattern 1.0 0.0 0.0 0.0 0.9
    Cairo.setSource pattern
    Cairo.fill

  -- Cairo.resetClip


-- |
arc :: Double -> Double -> Double -> Complex Double -> Cairo.Render ()
arc r α β (cx:+cy) = Cairo.arc cx cy r α β


-- |
circle :: Double -> Complex Double -> Cairo.Render ()
circle r centre = arc r 0 π centre


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
