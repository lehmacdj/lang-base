{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Lang.Common.Decorated where

import Data.Functor.Foldable

data DecoratedF f a b = f b ::@ Maybe a deriving (Functor)

-- cofree, explicitly for tags, with an additional maybe thrown in
data Decorated f a = f (Decorated f a) :@ Maybe a

type instance Base (Decorated f a) = DecoratedF f a
instance Functor f => Recursive (Decorated f a) where
    project (xs :@ x) = xs ::@ x
instance Functor f => Corecursive (Decorated f a) where
    embed (xs ::@ x) = xs :@ x
