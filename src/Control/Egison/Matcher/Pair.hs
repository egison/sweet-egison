-- |
--
-- Module:      Control.Egison.Matcher.Pair
-- Description: Matchers for pairs
-- Stability:   experimental

module Control.Egison.Matcher.Pair
  ( Pair(..)
  , tuple2
  , tuple2M
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search
import           Control.Egison.Match
import           Control.Egison.Matcher
import           Control.Egison.QQ

data Pair m1 m2 = Pair m1 m2

instance (Matcher m1 t1, Matcher m2 t2) => Matcher (Pair m1 m2) (t1, t2)

tuple2 :: Pattern (PP t1, PP t2) (Pair m1 m2) (t1, t2) (t1, t2)
tuple2 _ (Pair _ _) (t1, t2) = pure (t1, t2)

tuple2M :: Pair m1 m2 -> (t1, t2) -> (m1, m2)
tuple2M (Pair m1 m2) _ = (m1, m2)

instance (Eq a1, Matcher m1 a1, ValuePattern m1 a1, Eq a2, Matcher m2 a2, ValuePattern m2 a2) => ValuePattern (Pair m1 m2) (a1, a2) where
  value (e1, e2) () (Pair m1 m2) (v1, v2) = if eqAs m1 e1 v1 && eqAs m2 e2 v2 then pure () else mzero

eqAs :: (Matcher m t, ValuePattern m t) => m -> t -> t -> Bool
eqAs m x y =
  match dfs y m
    [ [mc| #x -> True |]
    , [mc| _ -> False |]
    ]
