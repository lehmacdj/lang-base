{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Common.Evaluate where

class Evaluable t v where
    evaluate :: t -> v
