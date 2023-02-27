{-# LANGUAGE OverloadedStrings #-}

module Language.Nano2.Reduce
  ( execFile,
    execString,
    execExpr,
    reduce1,
    e2,
    e5,
    parse,
  )
where

import Control.Exception (catch, throw)
import Language.Nano2.Parser
import Language.Nano2.Types

-- *** Problem 2: ONE STEP REDUCTION *********************************

--------------------------------------------------------------------------------
execFile :: FilePath -> IO (Maybe Expr)
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString)

--------------------------------------------------------------------------------
execString :: String -> IO (Maybe Expr)
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s)

--------------------------------------------------------------------------------
execExpr :: Expr -> IO (Maybe Expr)
--------------------------------------------------------------------------------
execExpr e = return (reduce1 e)

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

--------------------------------------------------------------------------------

-- | `isValue e` returns True or False depending on whether an
--   expression `e` is already a value according to our definition
--   of Nano2 in lecture. As a warm-up implement this function:
isValue :: Expr -> Bool
-- Per lecture, our "Values" are numbers or lambda terms
-- Everything else is not a final value
isValue (ENum _) = True
isValue (ELam _ _) = True
isValue _ = False

--------------------------------------------------------------------------------

-- | `subst x v e` returns the expression `e` with every free
--   occurrence of `x` replaced by the value `v`, which we have
--   denoted as `e[x := v]`, and defined in the lectures.

--   We're treating our values as a subset of expressions so the type
--   of `subst` is `Id -> Expr -> Expr -> Expr`, so if the argument
--   `v` were to have free variables we could end up with capture and
--   and invalid substitution (which would be bad)
--   but you can assume that `subst x v e` is only ever called where
--   the argument `v` is legitimately a value.

--   Implement this function (following the lecture notes)

subst :: Id -> Expr -> Expr -> Expr
-- following lecture 16, slide 18
subst x v (EVar var)
  | var == x = v -- x[x := v] = v
  | otherwise = EVar var -- y[x := v] = y when x /= y

subst _ _ (ENum n) = ENum n -- n[x := v] = n

-- (e1 + e2)[x := v] = e1[x := v] + e2[x := v]
subst x v (EBin op e1 e2) = EBin op (subst x v e1) (subst x v e2)

subst x v (ELet var def stmt)
  -- (let x = e1 in e2)[x := v] => let x = e1[x := v] in e2
  | x == var = ELet var (subst x v def) stmt
  -- (let y = e1 in e2)[x := v] => let x = e1[x := v] in e2[x := v]
  -- when x /= y
  | otherwise = ELet var (subst x v def) (subst x v stmt)

-- for ELam - replace the free variables in body
subst x v (ELam var body)
  | x == var = ELam var body -- (\x -> e)[x := v] => \x -> e, x is not free in e
                             -- because ELam provides the binding for x!
  | otherwise = ELam var (subst x v body) -- (\y -> e)[x := v] => \y -> e[x := v]

-- for EApp, just recurse inwards, no IDs to check
subst s v (EApp func arg) = EApp (subst s v func) (subst s v arg)

--------------------------------------------------------------------------------

-- | `reduce1 e` reduces the Nano expression `e` by exactly one
--   step according to the "small step" operational semantics
--   given in lecture. In fitting with this approach, we're not
--   interested in the final compiled values right now. Hence, we'll
--   treat values as a subset of our `Expr` objects, not as something
--   else.

--   Unlike the `eval` function from the lecture/homework, there's
--   no environment argument here and no need keep track of function
--   closures. Why not? Because we immediately substitute for bound
--   variables when recursing into a function/let body instead of
--   building an environment and deferring substitution until later.

--   Now, not every Expr can be reduced by one step; the terms that we
--   cannot reduce are the values and the stuck terms. So that means
--   that `reduce1` may fail. Unlike Java, we cannot use null in Haskell.
--   Instead Haskell provides a built-in datatype `Maybe` to allow for the
--   absence of a value or a function that may fail. It is defined as
--
--   data Maybe a = Nothing | Just a
--
--   Thus our `reduce1` function will have output type Maybe Expr. For
--   instance:
--     * `reduce1 (ENum 5)` should return Nothing because (ENum 5)
--       is a value.
--     * `reduce1 (EBin Plus (ENum 2) (ENum 2))` should return
--       `Just (ENum 4)`.
--
--   Implement the `reduce1` function. For stuck terms, like those
--   that are ill-typed (e.g. `EApp (ENum 1) (Num 2)`) or those with
--   undefined variables (e.g `EVar "cat"`) `reduce1` should return
--   Nothing instead of throwing an error.
--
--   Don't worry about recursive function definitions.

reduce1 :: Expr -> Maybe Expr
reduce1 (ENum _) = Nothing -- already a value
reduce1 (ELam _ _) = Nothing -- already a value

reduce1 (EVar _) = Nothing -- if it's just a Var by itself, it's stuck

-- Add: n1 + n2 => n
reduce1 (EBin op (ENum e1) (ENum e2)) = Just (ENum (e1 `opFunc` e2))
  where
    opFunc = case op of
      Plus -> (+)
      Minus -> (-)
      Mul -> (*)

-- Add-R: n1 + e2 => n1 + e2', try to reduce e2
reduce1 (EBin op (ENum e1) e2) = case reduce1 e2 of
    Just e2' -> Just (EBin op (ENum e1) e2')
    Nothing -> Nothing

-- Add-L: e1 + e2 => e1' + e2, try to reduce e1
reduce1 (EBin op e1 e2) = case reduce1 e1 of
  Just e1' -> Just (EBin op e1' e2)
  Nothing -> Nothing

reduce1 (ELet x e1 e2)
  -- Let: let x = v in e2 => e2[x := v]
  | isValue e1 = Just (subst x e1 e2)
  -- Let-Def: let x = e1 in e2 => let x = e1' in e2, try to reduce e1
  | otherwise = case reduce1 e1 of
    Just e1' -> Just (ELet x e1' e2)
    Nothing -> Nothing

-- App: (\x -> e) v => e[x := v]
reduce1 (EApp (ELam x e1) e2)
  | isValue e2 = Just (subst x e2 e1)

reduce1 (EApp e1 e2)
  -- App-R: v e => v e', try to reduce e2
  | isValue e1 =  case reduce1 e2 of
    Just e2' -> Just (EApp e1 e2')
    Nothing -> Nothing
  -- App-L: e1 e2 => e1' e2
  | otherwise = case reduce1 e1 of
    Just e1' -> Just (EApp e1' e2)
    Nothing -> Nothing


-- Here are some test cases!
--
-- (A) Numbers and Binary Operators
--
-- >>> reduce1  (EBin Plus (ENum 2) (ENum 3))
-- Just (ENum 5)
--
-- >>> reduce1  (ENum 5)
-- Nothing
--
-- >>> reduce1  (EVar "cat")
-- Nothing
--
-- >>> reduce1  (EBin Minus (ENum 2) (ELam "x" (EVar "x")))
-- Nothing

e1 = EBin Plus (EVar "x") (EVar "y")

e2 = ELet "x" (ENum 1) (ELet "y" (ENum 2) e1)

-- (B) Let bindings
--
-- >>> reduce1 (ELet "x" (EBin Plus (ENum 100) (ENum 2)) (EVar "x"))
-- Just (ELet "x" (ENum 102) (EVar "x"))
--
-- >>> reduce1 e2
-- Just (ELet "y" (ENum 2) (EBin Plus (ENum 1) (EVar "y")))
--
-- >>> reduce1 (ELet "y" (ENum 2) (EBin Plus (ENum 1) (EVar "y")))
-- Just (EBin Plus (ENum 1) (ENum 2))
--
-- (C) Functions and Applications
--
-- >>> reduce1 (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (ENum 3))
-- Just (EBin Plus (ENum 3) (ENum 3))

e3 = ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) (EApp (EVar "f") (EVar "h"))

e4 = ELet "x" (ENum 100) e3

e5 = ELet "f" (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) e4

-- >>> reduce1 e5
-- Just (ELet "x" (ENum 100)
--        (ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y")))
--          (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h"))))
--
-- >>> reduce1 (ELet "x" (ENum 100)
--               (ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y")))
--                 (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h"))))
-- Just (ELet "h" (ELam "y" (EBin Plus (ENum 100) (EVar "y")))
--        (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h")))
--
-- >>> reduce1 (ELet "h" (ELam "y" (EBin Plus (ENum 100) (EVar "y")))
--               (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h")))
-- Just (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2))))
--          (ELam "y" (EBin Plus (ENum 100) (EVar "y"))))
--
-- >>> reduce1 (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2))))
--               (ELam "y" (EBin Plus (ENum 100) (EVar "y"))))
-- Just (ELet "x" (ENum 0) (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2)))
--
-- >>> reduce1 (ELet "x" (ENum 0) (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2)))
-- Just (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2))
--
-- >>> reduce1 (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2))
-- Just (EBin Plus (ENum 100) (ENum 2))