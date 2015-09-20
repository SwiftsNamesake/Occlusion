-- |
-- Module      : Occlusion.Window
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
module Occlusion.Window where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Occlusion.Types
import qualified Occlusion.Core   as Core
import qualified Occlusion.Events as Events



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
create :: Complex Double -> IO (Window, DrawingArea)
create (winx:+winy) = do
  initGUI
  window <- windowNew
  frame  <- frameNew

  set window [windowTitle := "Occlusion"]

  canvas <- drawingAreaNew
  containerAdd frame canvas
  set window [ containerChild := frame ]
  windowSetDefaultSize window (round winx) (round winy)
  -- windowSetIconFromFile window "assets/images/gclef.png"

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  widgetShowAll window

  return (window, canvas)
