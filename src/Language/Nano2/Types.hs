{-# LANGUAGE OverloadedStrings #-}

module Language.Nano2.Types where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import qualified Data.List as L

import           Control.Exception
import           Data.Typeable

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

data Binop
  = Plus
  | Minus
  | Mul
  deriving (Eq, Ord, Show)

type Id = String

instance IsString Expr where
  fromString = EVar

data Expr
  = ENum Int
  | EVar Id
  | EBin Binop Expr Expr
  | ELet Id   Expr  Expr
  | EApp Expr Expr
  | ELam Id   Expr
  deriving (Eq, Show)

binopString :: Binop -> String
binopString Plus  = "+"
binopString Minus = "-"
binopString Mul   = "*"

exprString :: Expr -> String
exprString (ENum i)       = printf "%d" i
exprString (EVar x)       = x
exprString (EBin o e1 e2) = printf "(%s %s %s)" (show e1) (show o) (show e2)
exprString (ELet x e e')  = printf "let %s = %s in \n %s" x (show e) (show e')
exprString (EApp e1 e2)   = printf "(%s %s)" (show e1) (show e2)
exprString (ELam x e)     = printf "\\%s -> %s" x (show e)

--------------------------------------------------------------------------------
class Nano a where
  expr  :: a -> Expr

instance Nano Int where
  expr  = ENum