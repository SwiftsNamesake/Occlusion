-- |
-- Module      : Occlusion.Types
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
module Occlusion.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import qualified Data.Set as S
import qualified Data.Map as M

import Graphics.UI.Gtk
-- import Graphics.Rendering.Cairo



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
data AppState = AppState { _gui :: GUI, _animation :: AnimationData, _input :: InputData, _scene :: Scene }


-- |
data GUI = GUI { _window :: Window,  _canvas :: DrawingArea }


-- |
data InputData = InputData { _mouse :: Complex Double, _keyboard :: S.Set String }


-- |
data AnimationData = AnimationData { _fps :: Double, _frame :: Int }


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
type Polygon n = [Complex n]
-- type Scene n   = [Polygon n]


-- |
data Scene = Scene { _player :: Character, _obstacles :: [Polygon Double] }


-- |
data Character = Character { _position :: Complex Double, _velocity :: Complex Double }