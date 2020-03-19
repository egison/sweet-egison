module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  , tuple2
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad.Search           ( MonadSearch )


newtype Pair a b = Pair (a, b)

instance (Matcher a, Matcher b) => Matcher (Pair a b) where
  type Target (Pair a b) = (Target a, Target b)
  {-# INLINE wrap #-}
  wrap (a, b) = Pair (wrap a, wrap b)
  {-# INLINE unwrap #-}
  unwrap (Pair (a, b)) = (unwrap a, unwrap b)

{-# INLINABLE pair #-}
pair :: MonadSearch m => Pair a b -> m (a, b)
pair (Pair (x, y)) = pure (x, y)

{-# INLINABLE tuple2 #-}
tuple2 :: MonadSearch m => Pair a b -> m (a, b)
tuple2 = pair
