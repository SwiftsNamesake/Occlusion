-- |
-- Module      : Occlusion.Lenses
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
module Occlusion.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens
import Data.Complex
import Data.Functor
import qualified Data.Set as S
import qualified Data.Map as M

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import qualified Southpaw.Picasso.Palette as Palette

import Occlusion.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------
-- AppState --------------------------------------------------------------------------------------------------------------------------------

gui :: Lens AppState AppState GUI GUI
gui f s = (\new -> s { _gui=new }) <$> f (_gui s)


animation :: Lens AppState AppState AnimationData AnimationData
animation f s = (\new -> s { _animation=new }) <$> f (_animation s)


input :: Lens AppState AppState InputData InputData
input f s = (\new -> s { _input=new }) <$> f (_input s)


scene :: Lens AppState AppState Scene Scene
scene f s = (\new -> s { _scene=new }) <$> f (_scene s)

assets :: Lens AppState AppState Assets Assets
assets f s = (\new -> s { _assets=new }) <$> f (_assets s)


-- GUI -------------------------------------------------------------------------------------------------------------------------------------

window :: Lens GUI GUI Window Window
window f s = (\new -> s { _window=new }) <$> f (_window s)


canvas :: Lens GUI GUI DrawingArea DrawingArea
canvas f s = (\new -> s { _canvas=new }) <$> f (_canvas s)

-- InputData -------------------------------------------------------------------------------------------------------------------------------

mouse :: Lens InputData InputData (Complex Double) (Complex Double)
mouse f s = (\new -> s { _mouse=new }) <$> f (_mouse s)


click :: Lens InputData InputData (Maybe (Complex Double)) (Maybe (Complex Double))
click f s = (\new -> s { _click=new }) <$> f (_click s)


keyboard :: Lens InputData InputData (S.Set String) (S.Set String)
keyboard f s = (\new -> s { _keyboard=new }) <$> f (_keyboard s)

-- AnimationData ---------------------------------------------------------------------------------------------------------------------------

fps :: Lens AnimationData AnimationData Double Double
fps f s = (\new -> s { _fps=new }) <$> f (_fps s)


frame :: Lens AnimationData AnimationData Int Int
frame f s = (\new -> s { _frame=new }) <$> f (_frame s)

-- Scene -----------------------------------------------------------------------------------------------------------------------------------

player :: Lens Scene Scene Character Character
player f s = (\new -> s { _player=new }) <$> f (_player s)


obstacles :: Lens Scene Scene [Polygon Double] [Polygon Double]
obstacles f s = (\new -> s { _obstacles=new }) <$> f (_obstacles s)


-- Complex ---------------------------------------------------------------------------------------------------------------------------------


images :: Lens Assets Assets (M.Map String Cairo.Surface) (M.Map String Cairo.Surface)
images f s = (\new -> s { _images=new }) <$> f (_images s)



-- Character -------------------------------------------------------------------------------------------------------------------------------

position :: Lens Character Character (Complex Double) (Complex Double)
position f s = (\new -> s { _position=new }) <$> f (_position s)


velocity :: Lens Character Character (Complex Double) (Complex Double)
velocity f s = (\new -> s { _velocity=new }) <$> f (_velocity s)


name :: Lens Character Character String String
name f s = (\new -> s { _name=new }) <$> f (_name s)


colour :: Lens Character Character (Palette.Colour Double) (Palette.Colour Double)
colour f s = (\new -> s { _colour=new }) <$> f (_colour s)


behaviour :: Lens Character Character (Behaviour Character AppState Character) (Behaviour Character AppState Character)
behaviour f s = (\new -> s { _behaviour=new }) <$> f (_behaviour s)


-- Complex ---------------------------------------------------------------------------------------------------------------------------------

real :: Lens (Complex n) (Complex n) n n
real f (re:+im) = (:+im) <$> f re


imag :: Lens (Complex n) (Complex n) n n
imag f (re:+im) = (re:+) <$> f im

-- Lines -----------------------------------------------------------------------------------------------------------------------------------

linebegin :: Lens (Line a) (Line a) (Complex a) (Complex a)
linebegin f (Line fr to) = (`Line` to) <$> f fr


linestop :: Lens (Line a) (Line a) (Complex a) (Complex a)
linestop f (Line fr to) = (fr `Line`) <$> f to
