-- |
--
-- Module:      Control.Egison.Match
-- Description: Pattern-matching expressions
-- Stability:   experimental
--
-- This module exposes many combinators to perform pattern matching in [miniEgison](https://hackage.haskell.org/package/mini-egison)-like way.

module Control.Egison.Match
  ( matchAll
  , matchAllSingle
  , match
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..) )

{-# INLINE matchAll #-}
matchAll
  :: (Matcher m t, MonadSearch s)
  => ((m, t) -> s (m, t))
  -> t
  -> m
  -> [(m, t) -> s r]
  -> [r]
matchAll strategy target matcher =
  concatMap (\b -> toList (strategy (matcher, target) >>= b))

{-# INLINE matchAllSingle #-}
matchAllSingle
  :: (Matcher m t, MonadSearch s)
  => ((m, t) -> s (m, t))
  -> t
  -> m
  -> ((m, t) -> s r)
  -> [r]
matchAllSingle strategy target matcher b =
  toList (strategy (matcher, target) >>= b)

{-# INLINE match #-}
match
  :: (Matcher m t, MonadSearch s)
  => ((m, t) -> s (m, t))
  -> t
  -> m
  -> [(m, t) -> s r]
  -> r
match strategy target matcher bs = head (matchAll strategy target matcher bs)
