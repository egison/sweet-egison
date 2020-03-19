module Control.Egison.Matcher.UnorderedPair
  ( UnorderedPair(..)
  , upair
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad.Search           ( MonadSearch )
import           Control.Monad                  ( MonadPlus(..) )

import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )


newtype UnorderedPair a = UnorderedPair (a, a)

instance Matcher a => Matcher (UnorderedPair a) where
  type Target (UnorderedPair a) = (Target a, Target a)
  {-# INLINE wrap #-}
  wrap (a, b) = UnorderedPair (wrap a, wrap b)
  {-# INLINE unwrap #-}
  unwrap (UnorderedPair (a, b)) = (unwrap a, unwrap b)

instance Tuple2Pattern (UnorderedPair a) where
  type Fst (UnorderedPair a) = a
  type Snd (UnorderedPair a) = a

  {-# INLINABLE tuple2 #-}
  tuple2 (UnorderedPair (x, y)) = pure (x, y) `mplus` pure (y, x)

{-# INLINABLE upair #-}
upair :: MonadSearch m => UnorderedPair a -> m (a, a)
upair = tuple2
