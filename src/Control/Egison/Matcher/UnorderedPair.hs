{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Egison.Matcher.UnorderedPair
  ( UnorderedPair(..)
  , upair
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern             ( Pattern )
import           Data.Query.Pattern.Tuple       ( Tuple2Pattern(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Egison.Matcher.Pair    ( Pair )
import           Control.Egison.Match           ( match'
                                                , mc
                                                )


newtype UnorderedPair m = UnorderedPair m

instance Matcher m tgt => Matcher (UnorderedPair m) (tgt, tgt)

instance Matcher m tgt => Tuple2Pattern (UnorderedPair m) (tgt, tgt) where
  type Fst (tgt, tgt) = tgt
  type Snd (tgt, tgt) = tgt
  type FstTag (UnorderedPair m) = m
  type SndTag (UnorderedPair m) = m

  {-# INLINABLE tuple2 #-}
  tuple2 _ (x, y) = pure (x, y) `mplus` pure (y, x)

instance (Matcher m tgt, ValuePattern m tgt) => ValuePattern (UnorderedPair m) (tgt, tgt) where
  value a _ b =
    match' (a, b) @(Pair (UnorderedPair m) (UnorderedPair m))
      $  [mc| (($x, $y), (#x, #y)) -> pure () |]
      <> [mc| _ -> mzero |]

{-# INLINABLE upair #-}
upair
  :: Matcher m tgt => Pattern (UnorderedPair m) (tgt, tgt) '[m, m] '[tgt, tgt]
upair = tuple2
