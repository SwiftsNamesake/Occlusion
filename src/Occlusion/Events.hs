-- |
-- Module      : Occlusion.Events
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
module Occlusion.Events where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.IORef
import Control.Lens

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Occlusion.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Event listeners
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
onrender :: AppState -> Cairo.Render ()
onrender appstate = return ()


-- |
onanimate :: IORef AppState -> IO Bool
onanimate stateref = do
  appstate <- readIORef canvas
  widgetQueueDraw canvas
  modifyIORef stateref (animation.frame %~ (+1)) -- Increment frame count
  return True


-- |
onmousemoves :: AppState -> EventM EMotion Bool
onmousemoves appstate = return False


-- |
onmousepressed :: AppState -> EventM EButton Bool
onmousepressed appstate = return False


-- |
onmousereleased :: AppState -> EventM EButton Bool
onmousereleased appstate = return False


-- |
onkeypressed :: AppState -> EventM EKey Bool
onkeypressed appstate = return False


-- |
onkeyreleased :: AppState -> EventM EKey Bool
onkeyreleased appstate = return False


-- |
-- onwindowresized
-- onwindowminimised



--------------------------------------------------------------------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
attach :: Window -> DrawingArea -> IORef AppState -> IO ()
attach window canvas stateref = do
  appstate <- readIORef stateref

  canvas `on` draw               $ onrender appstate

  window `on` motionNotifyEvent  $ onmousemoves appstate
  window `on` buttonPressEvent   $ onmousepressed appstate
  window `on` buttonReleaseEvent $ onmousereleased appstate
  window `on` keyPressEvent      $ onkeypressed appstate
  window `on` keyReleaseEvent    $ onkeyreleased appstate

  timeoutAdd (onanimate stateref) (1.0 / appstate->animation.fps)

  return ()
