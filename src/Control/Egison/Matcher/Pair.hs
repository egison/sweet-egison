module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )
import           Data.Tagged                    ( Tagged(..) )


data Pair m1 m2 = Pair m1 m2

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Matcher (Pair m1 m2) (tgt1, tgt2)

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Tuple2Pattern (Pair m1 m2) (tgt1, tgt2) where
  type Fst (tgt1, tgt2) = tgt1
  type Snd (tgt1, tgt2) = tgt2
  type FstTag (Pair m1 m2) = m1
  type SndTag (Pair m1 m2) = m2

  {-# INLINABLE tuple2 #-}
  tuple2 (Tagged (x, y)) = pure (Tagged x, Tagged y)

{-# INLINABLE pair #-}
pair
  :: (Matcher m1 tgt1, Matcher m2 tgt2, MonadSearch s)
  => Tagged (Pair m1 m2) (tgt1, tgt2)
  -> s (Tagged m1 tgt1, Tagged m2 tgt2)
pair = tuple2
