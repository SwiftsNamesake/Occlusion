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
import Data.Functor
import Data.Function
import Control.Monad (forM, forM_, mapM, mapM_, when, unless, void)
import Control.Lens
import Text.Printf

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
    -- polygonDebug (thescene^.player.position) poly

  -- Render span lines
  -- forM (thescene^.obstacles) $ \poly -> do
  --   -- let Just (fr, to) = Core.anglespan (thescene^.player.position) poly
  --   --
  --   -- Cairo.setSourceRGBA 0.28 0.71 0.84 1.00
  --   -- Cairo.setLineWidth 2
  --   --
  --   -- vectorise Cairo.moveTo (thescene^.player.position)
  --   -- vectorise Cairo.lineTo $ snd fr
  --   -- Cairo.stroke
  --   --
  --   -- vectorise Cairo.moveTo (thescene^.player.position)
  --   -- vectorise Cairo.lineTo $ snd to
  --   -- Cairo.stroke

  -- Render shadows
  shadows thescene

  -- Render vertex markers
  -- forM (thescene^.obstacles) cornerMarkers


  -- Render player
  character (thescene^.player)
  return ()


-- |
cornerMarkers :: Polygon Double -> Cairo.Render ()
cornerMarkers poly = do
  Cairo.setSourceRGBA 0.94 0.06 0.05 1.00
  Cairo.setFontSize 16
  forM (zip [0..] poly) $ \(i, p) -> do
    vectorise Cairo.moveTo p
    Cairo.showText $ show i
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
  maybe pass line (Core.distantEdge pos poly)
  -- Cairo.liftIO $ print $ (uncurry ((,) `on` fst)) <$> Core.anglespan pos poly
  -- Cairo.setSourceRGBA 0.91 0.02 0.40 1.00
  -- Cairo.setLineWidth 8
  -- Cairo.stroke

  -- vectorise Cairo.moveTo $ pos
  vectorise Cairo.lineTo $ pos + mkPolar 1200 α
  arc 1200 (min α β) (max α β) pos
  vectorise Cairo.lineTo $ pos + mkPolar 1200 β
  -- Cairo.closePath
  -- vectorise Cairo.lineTo $ pos + mkPolar 1200 α
  -- arc 1200 (min α β) (max α β) pos
  -- arcDebug 1200 (min α β) (max α β) pos
  -- Cairo.setSourceRGBA 0.31 0.31 0.31 0.47
  -- Cairo.fill
  -- Cairo.clip

  when True $ Cairo.withRadialPattern px py 40 px py 1200 $ \pattern -> do
    Cairo.patternAddColorStopRGBA pattern 0.0 1.0 1.0 1.0 0.9
    Cairo.patternAddColorStopRGBA pattern 1.0 0.0 0.0 0.0 0.9
    Cairo.setSource pattern
    Cairo.fill

  Cairo.resetClip


-- |
arcDebug :: Double -> Double -> Double -> Complex Double -> Cairo.Render ()
arcDebug r α β centre = do
  arc r α β centre
  Cairo.setSourceRGBA 0.12 0.53 0.5 0.53
  Cairo.fill

  Cairo.setLineWidth 8
  arc r α β centre
  Cairo.stroke

  Cairo.setLineWidth 3
  arc (r*0.06) α β centre
  Cairo.stroke

  Cairo.setLineWidth 4
  arc (r*0.12) 0.0 α centre
  Cairo.setSourceRGBA 0.7 0.02 0.78 0.74
  Cairo.stroke

  Cairo.setLineWidth 4
  arc (r*0.18) 0.0 β centre
  Cairo.setSourceRGBA 0.08 0.42 0.15 0.51
  Cairo.stroke

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
polygonDebug :: Complex Double -> Polygon Double -> Cairo.Render ()
polygonDebug pos poly = do
  polygon poly >> Cairo.setSourceRGBA 0.38 0.84 0.09 0.67 >> Cairo.fill
  Cairo.setSourceRGBA 0.24 0.16 0.40 1.00
  Cairo.setFontSize 14
  forM (zip [(0 :: Int)..] poly) $ \(i, p) -> do
    vectorise Cairo.moveTo p
    Cairo.showText $ (printf "%d (%.02f°)" i (todeg . Core.normalise $ Core.angle pos p :: Double) :: String)
  return ()
  where
    todeg rad = rad * (180.0/π)
    torad deg = deg / (180.0/π)


-- |
line :: Edge Double -> Cairo.Render ()
line []      = return ()
line (e:dge) = void $ vectorise Cairo.moveTo e >> forM dge (vectorise Cairo.lineTo)


-- |
character :: Character -> Cairo.Render ()
character char = do
  Cairo.arc cx cy 12.0 0 (2*π)
  Cairo.setSourceRGBA 0.47 0.04 0.37 1.00
  Cairo.fill

  line [char^.position, char^.position + (800:+0)]
  Cairo.setSourceRGBA 1.00 0.00 0.00 1.00
  Cairo.setLineWidth  2.0
  Cairo.stroke

  line [char^.position, char^.position + (0:+800)]
  Cairo.setSourceRGBA 0.00 0.00 1.00 1.00
  Cairo.setLineWidth  2.0
  Cairo.stroke
  where
    cx:+cy = char^.position
