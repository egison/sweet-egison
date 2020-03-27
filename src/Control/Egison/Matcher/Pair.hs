module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Data.Query.Pattern             ( Pattern
                                                , ReturnList(..)
                                                )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )


data Pair m1 m2 = Pair m1 m2

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Matcher (Pair m1 m2) (tgt1, tgt2)

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Tuple2Pattern (Pair m1 m2) (tgt1, tgt2) where
  type Fst (tgt1, tgt2) = tgt1
  type Snd (tgt1, tgt2) = tgt2
  type FstTag (Pair m1 m2) = m1
  type SndTag (Pair m1 m2) = m2

  {-# INLINABLE tuple2 #-}
  tuple2 _ (x, y) = pure (x :- y :- Nil)

{-# INLINABLE pair #-}
pair
  :: (Matcher m1 tgt1, Matcher m2 tgt2)
  => Pattern (Pair m1 m2) (tgt1, tgt2) '[m1, m2] '[tgt1, tgt2]
pair = tuple2
