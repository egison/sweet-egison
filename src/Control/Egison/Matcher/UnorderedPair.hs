module Control.Egison.Matcher.UnorderedPair
  ( UnorderedPair(..)
  , upair
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )
import           Data.Tagged                    ( Tagged(..) )


newtype UnorderedPair m = UnorderedPair m

instance Matcher m tgt => Matcher (UnorderedPair m) (tgt, tgt)

instance Matcher m tgt => Tuple2Pattern (UnorderedPair m) (tgt, tgt) where
  type Fst _ = tgt
  type Snd _ = tgt
  type FstTag _ = m
  type SndTag _ = m

  {-# INLINABLE tuple2 #-}
  tuple2 (Tagged (x, y)) =
    pure (Tagged x, Tagged y) `mplus` pure (Tagged y, Tagged x)

{-# INLINABLE upair #-}
upair
  :: (Matcher m tgt, MonadSearch s)
  => Tagged (UnorderedPair m) (tgt, tgt)
  -> s (Tagged m tgt, Tagged m tgt)
upair = tuple2
