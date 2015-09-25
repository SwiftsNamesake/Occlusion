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
import qualified Data.Map as M

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Southpaw.Math.Constants
import qualified Southpaw.Picasso.Palette as Palette

import Occlusion.Types
import Occlusion.Vector
import qualified Occlusion.Window     as Window
import qualified Occlusion.Events     as Events
import qualified Occlusion.Behaviours as Behavours



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  (window, canvas) <- Window.create size
  assets'          <- loadAssets
  stateref         <- newIORef $ AppState { _animation = AnimationData { _fps=30.0, _frame=0 },
                                            _input     = InputData { _keyboard=S.empty, _mouse=0:+0, _click=Nothing },
                                            _scene     = Scene { _player=Character { _position=0:+0, _velocity=0:+0, _health=100, _name="Democritus", _colour=Palette.peru }, _obstacles=theobstacles },
                                            _gui       = GUI { _window=window, _canvas=canvas },
                                            _assets    = assets' }
  Events.attach window canvas stateref
  mainGUI
  where
    size = (720:+480) * 1.2


-- |
loadAssets :: IO Assets
loadAssets = do
  tree <- Cairo.imageSurfaceCreateFromPNG "assets/images/TreeAtDuskSmall.png"
  return $ Assets { _images=M.fromList [("tree", tree)] }

-- |
theobstacles :: [Polygon Double]
theobstacles = [[origin + let θ = α + n*2*π/ns in mkPolar mag θ | n <- [0..(ns-1)]] | (α, ns, mag, origin) <- [(0, 5, 78, 522:+150), (0, 8, 92, 192:+342)] ]
