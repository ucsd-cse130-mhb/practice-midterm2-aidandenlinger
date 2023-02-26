{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import           Language.Nano2.Types 
import qualified Language.Nano2.Desugar as Nano
import qualified Language.Nano2.FoldTables as Nano
import qualified Language.Nano2.Reduce as Nano

main :: IO ()
main = runTests [ unit ]

parse = Nano.parse

unit :: Score -> TestTree
unit sc = testGroup "NANO2"
  [ scoreTest ( uncurry (Nano.fold (\k v b -> k : b))
              , ([], Nano.t0)
              , (["aardvark","boa","cat"])
              , 5
              , "FoldTables fold 1")
  , scoreTest ( uncurry (Nano.fold (\k v b ->  v : b))
              , ([], Nano.t0)
              , ([100,20,30])
              , 5
              , "FoldTables fold 2")
  , scoreTest ( uncurry Nano.tmap
              , ((\k v -> if k == "cat" then v * 100 else v), Nano.t0)
              , (Nano.Bind "aardvark" 100 (Nano.Bind "boa" 20 (Nano.Bind "cat" 3000 Nano.Emp)))
              , 5
              , "FoldTables tmap 1")
  , scoreTest ( uncurry Nano.tmap
              , ((\k v -> v * 10), Nano.t0)
              , (Nano.Bind "aardvark" 1000 (Nano.Bind "boa" 200 (Nano.Bind "cat" 300 Nano.Emp)))
              , 5
              , "FoldTables tmap 2")

  , scoreTest ( Nano.maxKey
              , (Nano.Emp :: Nano.Table Int)
              , Nothing
              , 5
              , "FoldTables maxKey 1")
  , scoreTest ( Nano.maxKey
              , Nano.t0
              , Just "aardvark"
              , 10
              , "FoldTables maxKey 2")
  , scoreTest  ( Nano.mostFrequent
              , []
              , Nothing
              , 5
              , "FoldTables mostFrequent 1")
  , scoreTest  ( Nano.mostFrequent
              , ["cat", "dog", "cat", "cat", "dog", "mouse"]
              , Just "cat"
              , 10
              , "FoldTables mostFrequent 2")

  , scoreTest ( Nano.reduce1
              , (EBin Plus (ENum 2) (ENum 3))
              , Just (ENum 5)
              , 5
              , "Reduce A1")
  , scoreTest ( Nano.reduce1
              , (ENum 5)
              , Nothing
              , 5
              , "Reduce A2")
  , scoreTest ( Nano.reduce1
              , (EVar "cat")
              , Nothing
              , 5
              , "Reduce A3")              
  , scoreTest ( Nano.reduce1
              , (EBin Minus (ENum 2) (ELam "x" (EVar "x")))
              , Nothing
              , 5
              , "Reduce A4")
  , scoreTest ( Nano.reduce1
              , (ELet "x" (EBin Plus (ENum 100) (ENum 2)) (EVar "x"))
              , Just (ELet "x" (ENum 102) (EVar "x"))
              , 5
              , "Reduce B1")
  , scoreTest ( Nano.reduce1
              , Nano.e2
              , Just (ELet "y" (ENum 2) (EBin Plus (ENum 1) (EVar "y")))
              , 5
              , "Reduce B2")                            
  , scoreTest ( Nano.reduce1
              , (ELet "y" (ENum 2) (EBin Plus (ENum 1) (EVar "y")))
              , Just (EBin Plus (ENum 1) (ENum 2))
              , 5
              , "Reduce B3")       
  , scoreTest ( Nano.reduce1
              , (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (ENum 3))
              , Just (EBin Plus (ENum 3) (ENum 3))
              , 5
              , "Reduce C1")
  , scoreTest ( Nano.reduce1
              , Nano.e5
              , Just (ELet "x" (ENum 100) 
                       (ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) 
                         (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h"))))
              , 5
              , "Reduce C2")                            
  , scoreTest ( Nano.reduce1
              , (ELet "x" (ENum 100) 
                       (ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) 
                         (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h"))))
              , Just (ELet "h" (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) 
                       (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h")))
              , 5
              , "Reduce C3")
  , scoreTest ( Nano.reduce1
              , (ELet "h" (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) 
                       (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) (EVar "h")))
              , Just (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) 
                        (ELam "y" (EBin Plus (ENum 100) (EVar "y"))))
              , 5
              , "Reduce C4")      
  , scoreTest ( Nano.reduce1
              , (EApp (ELam "g" (ELet "x" (ENum 0) (EApp (EVar "g") (ENum 2)))) 
                        (ELam "y" (EBin Plus (ENum 100) (EVar "y"))))
              , Just (ELet "x" (ENum 0) (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2)))
              , 5
              , "Reduce C5")
  , scoreTest ( Nano.reduce1
              , (ELet "x" (ENum 0) (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2)))
              , Just (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2))
              , 5
              , "Reduce C6")      
  , scoreTest ( Nano.reduce1
              , (EApp (ELam "y" (EBin Plus (ENum 100) (EVar "y"))) (ENum 2))
              , Just (EBin Plus (ENum 100) (ENum 2))
              , 5
              , "Reduce C7")   

  , scoreTest ( Nano.noLets
              , (ELam "x" (EBin Mul (EVar "x") (EVar "y")))
              , True
              , 5
              , "noLets 1")
  , scoreTest ( Nano.noLets
              , (ELet "x" (ENum 1) (EBin Mul (EVar "x") (EVar "y")))
              , False
              , 5
              , "noLets 2")
  , scoreTest ( Nano.noLets
              , (EApp (ELam "x" (EVar "x")) (ELet "y" (ENum 2) (EVar "y")))
              , False
              , 5
              , "noLets 3")
  , scoreTest ( Nano.desugar
              , (ELam "x" (EBin Mul (EVar "x") (EVar "y")))
              , (ELam "x" (EBin Mul (EVar "x") (EVar "y")))
              , 10
              , "desugar 1")
  , scoreTest ( Nano.reduce1
              , (Nano.desugar (ELet "x" (ENum 2) (EBin Mul (ENum 3) (EVar "x"))))
              , Just (EBin Mul (ENum 3) (ENum 2))
              , 10
              , "desugar 2")
  , scoreTest ( \e -> Nano.reduce1 =<< Nano.reduce1 =<< (Nano.reduce1 (Nano.desugar e))
              , Nano.timesTwo
              , Just (EApp (ELam "x" (EBin Mul (ENum 42) (EVar "x"))) (ENum 2))
              , 10
              , "desugar 3")  
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    fileTest (f, r, n)  = scoreTest' sc (Nano.execFile, f, r, n, "file: " ++ f)
