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
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Set  as S
import Control.Lens
import Control.Monad (when, unless, forM, mapM)

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Occlusion.Types
import Occlusion.Lenses
import Occlusion.Vector
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
  --
  modifyIORef stateref (\appstate -> appstate & scene.player.velocity .~ currentVelocity appstate) -- TODO: This is ugly
  appstate <- readIORef stateref

  --
  widgetQueueDraw (appstate^.gui.canvas)
  modifyIORef stateref (animation.frame %~ (+1)) -- Increment frame count

  --
  let dt = (1.0/appstate^.animation.fps):+0
      v  = appstate^.scene.player.velocity
  modifyIORef stateref (scene.player.position %~ (+(dt*v)))
  return True


-- |
-- TODO: Move
currentVelocity :: AppState -> Complex Double
currentVelocity appstate
  | appstate^.input.click == Nothing = 0:+0
  | abs delta^.real < 12             = 0
  | otherwise                        = mkPolar 64 (phase delta)
  where
    delta = appstate^.input.mouse - appstate^.scene.player.position


-- |
onmousemoves :: IORef AppState -> EventM EMotion Bool
onmousemoves stateref = do
  p <- uncurry (:+) <$> eventCoordinates
  Cairo.liftIO $ do
    modifyIORef stateref (input.mouse .~ p)
  return False


-- |
onmousepressed :: IORef AppState -> EventM EButton Bool
onmousepressed stateref = do
  p <- uncurry (:+) <$> eventCoordinates
  Cairo.liftIO $ do
    modifyIORef stateref (input.click .~ Just p)
  return False


-- |
onmousereleased :: IORef AppState -> EventM EButton Bool
onmousereleased stateref = do
  p <- uncurry (:+) <$> eventCoordinates
  Cairo.liftIO $ do
    modifyIORef stateref (input.click .~ Nothing)
  return False


-- |
onkeypressed :: IORef AppState -> EventM EKey Bool
onkeypressed stateref = do
  key <- T.unpack <$> eventKeyName
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    unless (S.member key $ appstate^.input.keyboard) $ do
      modifyIORef stateref (input.keyboard %~ S.insert key)
  return False


-- |
onkeyreleased :: IORef AppState -> EventM EKey Bool
onkeyreleased stateref = do
  key <- T.unpack <$> eventKeyName
  Cairo.liftIO $ do
    appstate <- readIORef stateref
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

  canvas `on` motionNotifyEvent  $ onmousemoves stateref
  canvas `on` buttonPressEvent   $ onmousepressed stateref
  canvas `on` buttonReleaseEvent $ onmousereleased stateref
  window `on` keyPressEvent      $ onkeypressed stateref
  window `on` keyReleaseEvent    $ onkeyreleased stateref

  window `on` deleteEvent $ Cairo.liftIO mainQuit >> return False

  timeoutAdd (onanimate stateref) (round $ 1000.0 / appstate^.animation.fps)

  pass


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
velocityFromKey :: Double -> String -> Complex Double
velocityFromKey speed key = case key of
      "Left"  -> (-speed):+0
      "Right" -> ( speed):+0
      "Up"    ->       0 :+(-speed)
      "Down"  ->       0 :+( speed)
      _       ->       0:+0
