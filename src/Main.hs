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
    size = (720:+480) * 1.2


-- |
theobstacles :: [Polygon Double]
theobstacles = [[origin + let θ = α + n*2*π/ns in mkPolar mag θ | n <- [0..(ns-1)]] | (α, ns, mag, origin) <- [(0, 5, 78, 522:+150), (0, 8, 92, 192:+342)] ]