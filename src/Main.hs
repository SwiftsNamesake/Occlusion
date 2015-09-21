-- |
-- Module      : Main
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
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.IORef
import Data.Complex
import qualified Data.Set as S

import Graphics.UI.Gtk

import Occlusion.Types
import Occlusion.Vector
import qualified Occlusion.Window as Window
import qualified Occlusion.Events as Events



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  (window, canvas) <- Window.create size
  stateref         <- newIORef $ AppState { _gui       = GUI { _window=window, _canvas=canvas },
                                            _animation = AnimationData { _fps=30.0, _frame=0 },
                                            _input     = InputData { _keyboard=S.empty, _mouse=0:+0 },
                                            _scene     = Scene { _player=Character { _position=0:+0, _velocity=0:+0 }, _obstacles=theobstacles } }
  Events.attach window canvas stateref
  mainGUI
  where
    size = (720:+480)


-- |
theobstacles :: [Polygon Double]
theobstacles = [[(522:+150) + let θ = n*2*π/5 in 78*(cos θ:+sin θ) | n <- [0..4]], [(120:+342) + let θ = n*2*π/8 in 105*(cos θ:+sin θ) | n <- [0..7]]]
