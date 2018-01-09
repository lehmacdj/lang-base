{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module STLC where

import Data.Functor.Foldable
import Lang.Common.Decorated
import Lang.Common.AbstractInterpretation

data TermF b v a = Var v
                 | Constant b
                 | Lam v a
                 | App a a

data TypeF b a = Base b
               | Arr a a

data Constants = Unit
data BaseTypes = UnitTy

type Type = Fix (TypeF BaseTypes)

type Ident = Integer

type Term a = Decorated (TermF Constants Ident) a

data TyEq = TyEq Type Type

instance Abstractable (Term Type) [TyEq] where
    abstract 
