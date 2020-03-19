module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad.Search           ( MonadSearch )

import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )


newtype Pair a b = Pair (a, b)

instance (Matcher a, Matcher b) => Matcher (Pair a b) where
  type Target (Pair a b) = (Target a, Target b)
  {-# INLINE wrap #-}
  wrap (a, b) = Pair (wrap a, wrap b)
  {-# INLINE unwrap #-}
  unwrap (Pair (a, b)) = (unwrap a, unwrap b)

instance Tuple2Pattern (Pair a b) where
  type Fst (Pair a b) = a
  type Snd (Pair a b) = b

  {-# INLINABLE tuple2 #-}
  tuple2 (Pair (x, y)) = pure (x, y)

{-# INLINABLE pair #-}
pair :: MonadSearch m => Pair a b -> m (a, b)
pair = tuple2
