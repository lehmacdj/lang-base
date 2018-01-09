{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Common.AbstractInterpretation where

class Abstractable c a where
    abstract :: c -> a
