-- |
--
-- Module:      Control.Egison.Matcher.Pair
-- Description: Pair matcher
-- Stability:   experimental
--
-- This module defines 'Pair' matcher and operations on it.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern             ( Pattern )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Egison.Match           ( match'
                                                , mc
                                                )


-- | Simple matcher for pairs.
data Pair m1 m2 = Pair m1 m2

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Matcher (Pair m1 m2) (tgt1, tgt2)

instance (Matcher m1 tgt1, Matcher m2 tgt2) => Tuple2Pattern (Pair m1 m2) (tgt1, tgt2) where
  type Target2_1 (tgt1, tgt2) = tgt1
  type Target2_2 (tgt1, tgt2) = tgt2
  type Tag2_1 (Pair m1 m2) = m1
  type Tag2_2 (Pair m1 m2) = m2

  {-# INLINABLE tuple2 #-}
  tuple2 _ (x, y) = pure (x, y)

instance (Matcher m1 tgt1, Matcher m2 tgt2, ValuePattern m1 tgt1, ValuePattern m2 tgt2) => ValuePattern (Pair m1 m2) (tgt1, tgt2) where
  value a _ b =
    match' (a, b) @(Pair (Pair m1 m2) (Pair m1 m2))
      $  [mc| (($x, $y), (#x, #y)) -> pure () |]
      <> [mc| _ -> mzero |]

{-# INLINABLE pair #-}
-- | Type-specialized alias to 'tuple2'.
pair
  :: (Matcher m1 tgt1, Matcher m2 tgt2)
  => Pattern (Pair m1 m2) (tgt1, tgt2) '[m1, m2] '[tgt1, tgt2]
pair = tuple2
