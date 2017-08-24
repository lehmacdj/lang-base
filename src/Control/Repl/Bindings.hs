{-# LANGUAGE FlexibleContexts #-}

module Control.Repl.Bindings where

import Control.Arrow (left)
import Text.Parsec
import Text.Parsec.String
import Control.Repl
import Lang.Common.Variable
import Data.Bifunctor
import Control.Monad.Trans.Class
import Control.Monad.State

type Bindings v t = [(v, t)]

type BRepl v t = Repl (Bindings v t)

data Command v t = Assign v t
                 | Eval t
                 | Quit

whiteSpace :: Parser ()
whiteSpace = many (oneOf " \t\n") *> pure ()

pCommand :: Parser v -> String -> Parser t -> Parser (Command v t)
pCommand pv a pt =
    (string ":q" *> pure Quit)
    <|> (Assign <$> pv <*> (whiteSpace *> string a *> whiteSpace *> pt))
    <|> (Eval <$> pt)

applySubsts :: Substitutable v t t => t -> Bindings v t -> t
applySubsts = foldr (uncurry substitute)

-- this creates a repl with a few default commands:
-- ":q" - quits the repl
-- "<v> <a> <t>" - assigns v to t
-- "<t>" - evaluates t with substitutions substituted
doBindRepl :: Substitutable v t t
           => String
           -> Parser v
           -> String
           -> Parser t
           -> (t -> BRepl v t ())
           -> IO ()
doBindRepl t pv a pt r = doRepl t pc rc []
    where pc = first show <$> parse (pCommand pv a pt) t
          rc Quit cont = pure ()
          rc (Eval t) cont = do
              bs <- get
              r (t `applySubsts` bs)
              cont
          rc (Assign v t) cont = do
              bs <- get
              put $ (v, t):bs
              cont
