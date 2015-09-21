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
import Data.Complex
import Data.Functor
import qualified Data.Text as T
import qualified Data.Set  as S
import Control.Lens
import Control.Monad (when, unless)

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Occlusion.Types
import Occlusion.Lenses
import qualified Occlusion.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Event listeners
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
onrender :: AppState -> Cairo.Render ()
onrender appstate = do
  Render.scene (appstate^.scene)


-- |
onanimate :: IORef AppState -> IO Bool
onanimate stateref = do
  appstate <- readIORef stateref
  widgetQueueDraw (appstate^.gui.canvas)
  modifyIORef stateref (animation.frame %~ (+1)) -- Increment frame count

  let dt = (1.0/appstate^.animation.fps):+0
      v  = appstate^.scene.player.velocity
  modifyIORef stateref (scene.player.position %~ (+(dt*v)))
  return True
  -- where
    -- dt = 1.0/appstate^.animation.fps


-- |
onmousemoves :: IORef AppState -> EventM EMotion Bool
onmousemoves stateref = return False


-- |
onmousepressed :: IORef AppState -> EventM EButton Bool
onmousepressed stateref = return False


-- |
onmousereleased :: IORef AppState -> EventM EButton Bool
onmousereleased stateref = return False


-- |
onkeypressed :: IORef AppState -> EventM EKey Bool
onkeypressed stateref = do
  key <- T.unpack <$> eventKeyName
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    unless (S.member key $ appstate^.input.keyboard) $ do
      modifyIORef stateref (input.keyboard %~ S.insert key)
      modifyIORef stateref (scene.player.velocity %~ (+ velocityFromKey 42 key))
  return False


-- |
onkeyreleased :: IORef AppState -> EventM EKey Bool
onkeyreleased stateref = do
  key <- T.unpack <$> eventKeyName
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    modifyIORef stateref (scene.player.velocity %~ (subtract $ velocityFromKey 42 key))
    modifyIORef stateref (input.keyboard %~ S.delete key)
  return False


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

  canvas `on` draw               $ Cairo.liftIO (readIORef stateref) >>= onrender

  window `on` motionNotifyEvent  $ onmousemoves stateref
  window `on` buttonPressEvent   $ onmousepressed stateref
  window `on` buttonReleaseEvent $ onmousereleased stateref
  window `on` keyPressEvent      $ onkeypressed stateref
  window `on` keyReleaseEvent    $ onkeyreleased stateref

  window `on` deleteEvent $ Cairo.liftIO mainQuit >> return False

  timeoutAdd (onanimate stateref) (round $ 1000.0 / appstate^.animation.fps)

  return ()


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
velocityFromKey :: Double -> String -> Complex Double
velocityFromKey speed key = case key of
      "Left"  -> (-speed):+0
      "Right" -> ( speed):+0
      "Up"    ->       0 :+(-speed)
      "Down"  ->       0 :+( speed)
      _       ->       0:+0
