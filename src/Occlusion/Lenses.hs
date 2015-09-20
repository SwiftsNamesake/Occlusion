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
-- {-# LANGUAGE TemplateHaskell #-}



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


-- GUI -------------------------------------------------------------------------------------------------------------------------------------
window :: Lens GUI GUI Window Window
window f s = (\new -> s { _window=new }) <$> f (_window s)


canvas :: Lens GUI GUI DrawingArea DrawingArea
canvas f s = (\new -> s { _canvas=new }) <$> f (_canvas s)


-- InputData -------------------------------------------------------------------------------------------------------------------------------
mouse :: Lens InputData InputData (Complex Double) (Complex Double)
mouse f s = (\new -> s { _mouse=new }) <$> f (_mouse s)


keyboard :: Lens InputData InputData (S.Set String) (S.Set String)
keyboard f s = (\new -> s { _keyboard=new }) <$> f (_keyboard s)


-- AnimationData ---------------------------------------------------------------------------------------------------------------------------
fps :: Lens AnimationData AnimationData Double Double
fps f s = (\new -> s { _fps=new }) <$> f (_fps s)


frame :: Lens AnimationData AnimationData Int Int
frame f s = (\new -> s { _frame=new }) <$> f (_frame s)
