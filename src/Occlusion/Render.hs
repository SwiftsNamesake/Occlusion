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

  character (thescene^.player)
  forM (thescene^.obstacles) $ \poly -> do
    let Just (fr, to) = Core.anglespan (thescene^.player.position) poly
    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo fr

    vectorise Cairo.moveTo (thescene^.player.position)
    vectorise Cairo.lineTo to

    Cairo.setSourceRGBA 0.28 0.71 0.84 1.00
    Cairo.stroke

  return ()



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
