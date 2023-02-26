{-# LANGUAGE OverloadedStrings #-}

module Language.Nano2.Desugar
  ( noLets , desugar , timesTwo )
  where

import Control.Exception (throw, catch)
import Language.Nano2.Reduce
import Language.Nano2.Types
import Language.Nano2.Parser

-- *******************************************************************
-- *** Problem 3: REMOVING THE LET BINDINGS **************************
-- *******************************************************************

-- Warm-up: Implement a function `noLets` where `noLets e` returns
--    `True` if the input `e` does not contain any `Let` bindings
--    and `False` if it does. We'll use it later.

noLets :: Expr -> Bool
noLets e = error "TBD: noLets implementation"

-- Test Cases
--
-- >>> noLets (ELam "x" (EBin Mul (EVar "x") (EVar "y")))
-- True
--
-- >>> noLets (ELet "x" (ENum 1) (EBin Mul (EVar "x") (EVar "y")))
-- False
--
-- >>> noLets (EApp (ELam "x" (EVar "x")) (ELet "y" (ENum 2) (EVar "y")))
-- False

--  We mentioned at various points in the lectures that 
--    language features like let-bindings are just syntactic
--    sugar. The let-bindings can even be replaced with
--    lambda abstractions and function application without 
--    using any Church encodings or anything complicated. Think
--    about how a let-binding is a combination of a function
--    definition and the immediate application of it.
--
--    Implement a function `desugar` that takes an Expr as input
--    and outputs an Expr without any Let bindings that behaves
--    identically to the input under evaluation;
--    i.e. for any `e` we must have `noLets (desugar e)`
--    returns True`.

desugar :: Expr -> Expr
desugar e = error "TBD: desugar implementation"

-- Test Cases:
-- We can't give you any interesting outputs directly because that
--   would give away too much of the answer.
--
-- >>> desugar (ELam "x" (EBin Mul (EVar "x") (EVar "y")))
-- ELam "x" (EBin Mul (EVar "x") (EVar "y"))
--
-- >>> reduce1 (desugar (ELet "x" (ENum 2) (EBin Mul (ENum 3) (EVar "x"))))
-- Just (EBin Mul (ENum 3) (ENum 2))

timesTwo = ELet "c" (ENum 42) 
                (ELet "cTimes" (ELam "x" (EBin Mul (EVar "c") (EVar "x"))) 
                      (ELet "c" (ENum 5) (EApp (EVar "cTimes") (ENum 2))))

-- >>> reduce1 =<< reduce1 =<< (reduce1 (desugar timesTwo))
-- Just (EApp (ELam "x" (EBin Mul (ENum 42) (EVar "x"))) (ENum 2))