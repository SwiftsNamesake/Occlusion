-- |
-- Module      : Occlusion.TH
-- Description : Experimental Template Haskell module
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created Jonatan H Sundqvist 2015
-- Based on https://github.com/ekmett/lens/blob/ec19f31617d8c826f4f1bb0196b3ba94bf94c0cc/src/Control/Lens/Internal/FieldTH.hs

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Occlusion.TH where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Language.Haskell.TH



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
makeLenses :: Name -> Q DescQ
makeLenses name = do
  info <- reify name
  case info of
    TyConI desc -> makeFieldOpticsFor desc
    _           -> "Expected type constructor name"


-- |
makeFieldOpticsFor :: Dec -> DescQ
makeFieldOpticsFor desc = case descr of
  DataD        _ tyName vars cons _ -> makeFieldOpticsForDec' tyName (mkS tyName vars)        cons
  NewTypeD     _ tyName vars cons _ -> makeFieldOpticsForDec' tyName (mkS tyName vars)        [con]
  DataInstD    _ tyName args cons _ -> makeFieldOpticsForDec' tyName (tyName `conAppsT` args) cons
  NewTypeInstD _ tyName args con  _ -> makeFieldOpticsForDec' tyName (tyName `conAppsT` args) [con]
  where
    mkS tyName vars = tyName `conAppsT` map VarT (toListOf typeVars vars)


-- |
makeFieldOpticsForDec' :: Name -> Type -> [Con] -> DescQ
makeFieldOpticsForDec' tyName s cons = do
  fieldCons <- traverse normalizeConstructor cons
  let allFields = toListOf (folded . _2 . folded . _1 . folded) fieldCons
  let defCons   = over normFieldLabels (expandName allfields) fieldCons
      allDefs   = setOf (normFieldLabels . folded) defCons
  perDef <- T.sequenceA (fromSet (buildScaffold s defCons) allDefs)

  let defs = Map.toList perDef
  case _classyLenses tyName of
    Just (className, methodName) -> _
    Nothing                      -> do
