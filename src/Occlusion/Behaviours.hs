-- |
-- Module      : Occlusion.Behaviours
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 25 2015

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
module Occlusion.Behaviours where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Maybe   (fromMaybe)

import Control.Lens

import Southpaw.Math.Constants
import Southpaw.Math.Trigonometry

import           Occlusion.Types
import qualified Occlusion.Lenses as L
import qualified Occlusion.Core   as Core



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
player :: Behaviour Character AppState Character
player self appstate = self & foldr (.) id [L.position %~ (+(dt*v)), L.velocity .~ currentVelocity appstate]
  where
    dt = (1.0/appstate^.L.animation.L.fps):+0
    v  = self^.L.velocity
  -- modifyIORef stateref (\appstate -> appstate & scene.player.velocity .~ currentVelocity appstate) -- TODO: This is ugly


-- |
npc :: [Complex Double] -> Behaviour Character AppState Character
npc path self appstate = self & L.position %~ flip fromMaybe (Core.walkalong path (n*v*dt))
  where
    dt = (1.0/appstate^.L.animation.L.fps)
    v  = realPart . abs $ self^.L.velocity
    n  = fromIntegral $ appstate^.L.animation.L.frame

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make polymorphic
run :: Lens AppState AppState Character Character -> AppState -> AppState
run char appstate = appstate & char .~ (appstate^.char.L.behaviour) (appstate^.char) appstate

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Move
currentVelocity :: AppState -> Complex Double
currentVelocity appstate
  | appstate^.L.input.L.click == Nothing = 0.0:+0.0
  | abs delta^.L.real < 12.0             = 0.0
  | otherwise                            = mkPolar 64.0 (phase delta)
  where
    delta = appstate^.L.input.L.mouse - appstate^.L.scene.L.player.L.position
