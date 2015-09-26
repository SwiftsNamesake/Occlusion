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
{-# LANGUAGE Rank2Types #-}



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
import qualified Occlusion.Behaviours as Behaviours
import qualified Occlusion.Render     as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Event listeners
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
onrender :: AppState -> Cairo.Render ()
onrender appstate = do
  Render.background appstate
  Render.scene (appstate^.scene)


-- |
-- TODO: Simulation speed
-- TODO: Factour out 'tick'
-- TODO: Use state monad
onanimate :: IORef AppState -> IO Bool
onanimate stateref = do
  --
  appstate <- readIORef stateref

  --
  widgetQueueDraw (appstate^.gui.canvas)
  modifyIORef stateref (animation.frame %~ (+1)) -- Increment frame count

  -- TODO: Refactor (cf. State, traverse, each)
  -- modifyIORef stateref $ \appstate -> forM [scene.player, scene.npcs.ix 0] $ \char -> Behaviours.run char
  modifyIORef stateref $ Behaviours.run (scene.player)
  modifyIORef stateref $ Behaviours.run (scene.npcs.nth 0)
  return True
  where
    -- nth :: Functor f => Int -> (Character -> f Character) -> [Character] -> f [Character]
    nth :: Int -> Lens [Character] [Character] Character Character
    nth i f s = let assemble new = take i s ++ [new] ++ drop (i+1) s in assemble <$> f (s !! i)


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

  canvas `on` draw $ Cairo.liftIO (readIORef stateref) >>= onrender

  canvas `on` motionNotifyEvent  $ onmousemoves    stateref
  canvas `on` buttonPressEvent   $ onmousepressed  stateref
  canvas `on` buttonReleaseEvent $ onmousereleased stateref
  window `on` keyPressEvent      $ onkeypressed    stateref
  window `on` keyReleaseEvent    $ onkeyreleased   stateref

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
