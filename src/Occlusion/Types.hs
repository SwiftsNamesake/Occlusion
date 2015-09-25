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
import qualified Graphics.Rendering.Cairo as Cairo

import           Southpaw.Picasso.Palette (Colour)
import qualified Southpaw.Picasso.Palette as Palette



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data AppState = AppState { _gui :: GUI, _animation :: AnimationData, _input :: InputData, _scene :: Scene, _assets :: Assets }


-- |
data GUI = GUI { _window :: Window,  _canvas :: DrawingArea }


-- |
data InputData = InputData { _mouse :: Complex Double, _click :: Maybe (Complex Double), _keyboard :: S.Set String }


-- |
data AnimationData = AnimationData { _fps :: Double, _frame :: Int }


-- |
data Assets = Assets { _images :: M.Map String Cairo.Surface } --  _strings }


--------------------------------------------------------------------------------------------------------------------------------------------

-- |
type Polygon n = [Complex n]
type Edge n    = [Complex n]
type Linear n  = (n, n) -- TODO: Use custom type (eg. Linear { slope :: n, intercept :: n })
data Line n    = Line (Complex n) (Complex n)
-- type Scene n   = [Polygon n]


-- |
data Scene = Scene { _player :: Character, _obstacles :: [Polygon Double] }


-- |
-- TODO: Factor out stats, behaviour, visuals, 'physics', etc.
-- TODO: Should it be Behaviour Character AppState AppState (would allow player to modify world, maybe some restricted subset of it)
data Character = Character { _position :: Complex Double, _velocity :: Complex Double, _health :: Int, _name :: String, _colour :: Colour Double, _behaviour :: Behaviour Character AppState Character }


-- |
-- TODO: This type should be much more flexible
-- TOOD: Should this be a newtype or type synonym instead (?)
type Behaviour self s s' = self -> s -> s'
-- newtype Behaviour self s s' = Behaviour { _respond :: self -> s -> s' }
-- data Behaviour self s s' = Behaviour { _respond :: self -> s -> s' }
