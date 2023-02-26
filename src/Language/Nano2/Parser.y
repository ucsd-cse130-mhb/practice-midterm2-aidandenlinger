{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano2.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano2.Lexer
import Language.Nano2.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    in    { IN _     }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }


-- Operators
%right in
%nonassoc '=' 
%right '->'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

Expr : Expr '+'  Expr               { EBin Plus  $1 $3 }
     | Expr '-'  Expr               { EBin Minus $1 $3 }
     | Expr '*'  Expr               { EBin Mul   $1 $3 }
     | '\\' ID '->' Expr            { ELam $2 $4       }
     | let ID '='  Expr in Expr     { ELet $2 $4 $6    }
     | let ID Ids '=' Expr in Expr  { ELet $2 (mkLam $3 $5) $7 }
     | Axpr                         { $1               }

Axpr : Axpr Bxpr                   { EApp $1 $2       }
     | Bxpr                        { $1               }


Bxpr : TNUM                        { ENum $1        }
     | '(' Expr ')'                { $2             }
     | ID                          { EVar $1        }

Ids : ID                           { [$1]           }
    | ID Ids                       { $1 : $2        }

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
