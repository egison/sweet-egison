-- |
--
-- Module:      Control.Egison.Match
-- Description: Pattern-matching expressions
-- Stability:   experimental
--
-- This module exposes many combinators to perform pattern matching in [miniEgison](https://hackage.haskell.org/package/mini-egison)-like way.

{-# LANGUAGE TypeApplications #-}

module Control.Egison.Match
  ( matchAll
  , matchAllSingle
  , match
  )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..)
                                                , bfs
                                                , dfs
                                                )


{-# INLINE matchAll #-}
matchAll
  :: (Matcher m t, MonadSearch s)
  => ((m, t) -> s (m, t))
  -> t
  -> m
  -> [(m, t) -> s r]
  -> [r]
matchAll strategy target matcher bs =
  concatMap (\b -> toList (strategy (matcher, target) >>= b)) bs

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
