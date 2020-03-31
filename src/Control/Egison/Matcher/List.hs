-- |
--
-- Module:      Control.Egison.Matcher.List
-- Description: List matcher
-- Stability:   experimental
--
-- This module defines 'List' matcher and operations on it.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Egison.Matcher.List
  ( List(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Data.Query.Pattern.Tuple       ( tuple2 )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Egison.Matcher.Pair    ( Pair )
import           Control.Egison.Match           ( match'
                                                , mc
                                                )


-- | 'List' matcher is a matcher for collections that matches as if they're normal lists.
newtype List m = List m

instance Matcher m tgt => Matcher (List m) [tgt]

instance Matcher m tgt => CollectionPattern (List m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (List m) = m
  {-# INLINE nil #-}
  nil _ [] = pure ()
  nil _ _  = mzero
  {-# INLINE cons #-}
  cons _ []       = mzero
  cons _ (x : xs) = pure (x, xs)
  {-# INLINABLE join #-}
  join _ []       = pure ([], [])
  join t (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join t xs
    pure (x : ys, zs)
  {-# INLINABLE spread #-}
  spread _ []       = mzero
  spread t (x : xs) = pure (x : xs) `mplus` spread t xs

instance (Matcher m tgt, ValuePattern m tgt) => ValuePattern (List m) [tgt] where
  value a _ b =
    match' (a, b) @(Pair (List m) (List m))
      $  [mc| ([], []) -> pure () |]
      <> [mc| ($x : $xs, (#x : #xs)) -> pure () |]
      <> [mc| _ -> mzero |]
