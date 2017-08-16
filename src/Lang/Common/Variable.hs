{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Common.Variable where

class VarContaining t v where
    freeVars :: t -> [v]
    allVars :: t -> [v]

-- Substitutable v s t means that v can be substituted for s in t
class VarContaining t v => Substitutable v s t where
    -- substitute v s t computes t{s/v}
    substitute :: v -> s -> t -> t
