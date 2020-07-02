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
--  , matchAllDFS
  )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..)
                                                , bfs
                                                , dfs
                                                )
import           Control.Egison.QQ                  ( mc )


{-# INLINABLE matchAll #-}
matchAll
  :: (Matcher m t, MonadSearch s)
  => ((m, t) -> s (m, t))
  -> t
  -> m
  -> [(m, t) -> s r]
  -> [r]
matchAll strategy target matcher bs = concat (map (\b -> toList (strategy (matcher, target) >>= b)) bs)
