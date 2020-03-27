module Control.Egison.Matcher.UnorderedPair
  ( UnorderedPair(..)
  , upair
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern             ( Pattern
                                                , ReturnList(..)
                                                )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )


newtype UnorderedPair m = UnorderedPair m

instance Matcher m tgt => Matcher (UnorderedPair m) (tgt, tgt)

instance Matcher m tgt => Tuple2Pattern (UnorderedPair m) (tgt, tgt) where
  type Fst _ = tgt
  type Snd _ = tgt
  type FstTag _ = m
  type SndTag _ = m

  {-# INLINABLE tuple2 #-}
  tuple2 _ (x, y) = pure (x :- y :- Nil) `mplus` pure (y :- x :- Nil)

{-# INLINABLE upair #-}
upair
  :: Matcher m tgt => Pattern (UnorderedPair m) (tgt, tgt) '[m, m] '[tgt, tgt]
upair = tuple2
