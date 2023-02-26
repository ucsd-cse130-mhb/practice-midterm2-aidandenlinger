
module Language.Nano2.FoldTables where 

import qualified Data.List as L
    
-- *******************************************************************
-- *** Problem 1: FOLDS FOR TABLES ***********************************
-- *******************************************************************

{-
Consider the following data type to represent lookup tables 
mapping String keys to values of type a.
-}

type Key = String 

data Table a = Emp 
             | Bind Key a (Table a) 
  deriving (Eq)

instance Show a => Show (Table a) where
    show t = case toList t of
                []  -> "[]"
                kvs -> "[ " ++ L.intercalate ", " (map showB kvs) ++ " ]"
      where
          showB (k, v) = k ++ " -> " ++ show v

toList :: Table a -> [(Key, a)]
toList Emp = []
toList (Bind k v r) = (k, v) : toList r

-- Here are some example tables

t0 :: Table Int
t0 = Bind "aardvark" 100 (Bind "boa" 20 (Bind "cat" 30 Emp))

-- (A) Fold over a table  
-- Let's implement and use a `fold` function for the `Table`.
-- Note that the function for the fold can use both the 
-- key and the value.

fold :: (Key -> a -> b -> b) -> b -> Table a -> b
fold op b Emp          = error "TBD: fold empty"
fold op b (Bind k v t) = error "TBD: fold bind"

-- When you are done, you should see the following behavior

-- >>> fold (\k v b -> k : b) [] t0
-- ["aardvark","boa","cat"]

-- >>> fold (\k v b ->  v : b) [] t1
-- [100,20,30]

-- (B) Map using fold
-- Use fold to implement a `tmap` function for `Table`

tmap :: (Key -> a -> b) -> Table a -> Table b
tmap f t   = fold op base t
  where
      op   = error "TBD: tmap op"
      base = error "TBD: tmap base"

-- Note: You can add arguments to op or base.

-- When you are done you should see the following

-- >>> tmap (\k v -> if k == "cat" then v * 100 else v) t0
-- [ aardvark -> 100, boa -> 20, cat -> 3000 ]

-- >>> tmap (\k v -> v * 10) t0
-- [ aardvark -> 1000, boa -> 200, cat -> 300 ]

-- (C) Maximum Key using fold 
-- Haskell provides a built-in datatype `Maybe` to allow for the 
-- absence of a value. It is defined as
--
--   data Maybe a = Nothing | Just a
--
-- Use fold to implement a `maxKey` function such that
-- `maxKey t` evaluates to `Just key` where `key` has the
-- largest value in `t` or `Nothing` if `t` is empty.

maxKey :: (Ord a) => Table a -> Maybe Key
maxKey t   = post (fold op base t)
  where
      post = error "TBD: maxKey post"
      op   = error "TBD: maxKey op"
      base = error "TBD: maxKey base"

-- When you are done you should see the following

-- >>> maxKey Emp
-- Nothing

-- >>> maxKey t0
-- Just "aardvark"


-- (D) Most frequent word 
-- Use `foldr :: (a -> b -> b) -> b -> [a] -> b`
-- and the Table operations defined previously
-- to implement a function that returns the most 
-- frequent word in a list

mostFrequent :: [String] -> Maybe String
mostFrequent xs = post (foldr op base xs)
  where
      post      = error "TBD: mostFrequent post"
      op        = error "TBD: mostFrequent op"
      base      = error "TBD: mostFrequent base"

-- When you are done, you should see the following

-- >>> mostFrequent []
-- Nothing

-- >>> mostFrequent ["cat", "dog", "cat", "cat", "dog", "mouse"]
-- Just "cat"
