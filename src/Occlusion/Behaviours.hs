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

import Control.Lens

import Southpaw.Math.Constants
import Southpaw.Math.Trigonometry

import           Occlusion.Types
import qualified Occlusion.Lenses as L



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
player :: Behaviour Character AppState Character
player self appstate = self & L.position %~ (+(dt*v))
  where
    dt = (1.0/appstate^.L.animation.L.fps):+0
    v  = appstate^.L.scene.L.player.L.velocity

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make polymorphic
run :: Lens AppState AppState Character Character -> AppState -> AppState
run char appstate = appstate & char .~ (appstate^.char.L.behaviour) (appstate^.char) appstate
