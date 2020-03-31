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
  , match
  , matchAllDFS
  , matchDFS
  , mc
  -- * Flexible variants of 'match' and 'matchAll'
  , match'
  , matchAll'
  , mmatchAll'
  )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..)
                                                , BFS
                                                , DFS
                                                )
import           Data.Query                     ( Query
                                                , SearchStrategy
                                                , query
                                                , query'
                                                )
import           Data.Query.QQ                  ( q )


{-# INLINABLE match #-}
-- | 'match' returns the first matching result of 'matchAll'.
match
  :: Matcher matcher target
  => target
  -> matcher
  -> [Query BFS matcher target out]
  -> out
match tgt m = head . matchAll tgt m

{-# INLINABLE matchAll #-}
-- | Perform pattern-matching and return matching results as a list.
matchAll
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query BFS matcher target out]
  -> [out]
matchAll tgt _ qs = collect $ query @matcher (mconcat qs) tgt

{-# INLINABLE matchDFS #-}
-- | Equivalent to 'match' except that this uses 'DFS' for searching.
matchDFS
  :: Matcher matcher target
  => target
  -> matcher
  -> [Query DFS matcher target out]
  -> out
matchDFS tgt m = head . matchAllDFS tgt m

{-# INLINABLE matchAllDFS #-}
-- | Equivalent to 'matchAll' except that this uses 'DFS' for searching.
matchAllDFS
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query DFS matcher target out]
  -> [out]
matchAllDFS tgt _ qs = collect $ query @matcher (mconcat qs) tgt

-- | 'QuasiQuoter' for match clauses, an alias to 'q'.
mc :: QuasiQuoter
mc = q


{-# INLINABLE match' #-}
-- | Flexible variant of 'match'. We can specify the search strategy and the matcher using type applications.
match'
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> out
match' tgt = head . matchAll' tgt

{-# INLINABLE matchAll' #-}
-- | Flexible variant of 'matchAll'. We can specify the search strategy and the matcher using type applications.
--
-- e.g. @'matchAll'' \@'DFS' tgt \@('Control.Egison.Matcher.List.List' 'Control.Egison.Matcher.EqM') [q| _ ++ $x : #x : _ -> x |]@ finds all equal pairs in @tgt@.
matchAll'
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> [out]
matchAll' tgt qu = collect $ query @matcher qu tgt

{-# INLINABLE mmatchAll' #-}
-- | Equivalent to 'matchAll'' except that this returns results as 'MonadSearch' value.
mmatchAll'
  :: forall strategy matcher target out
   . (Matcher matcher target, MonadSearch strategy)
  => target
  -> forall m
   . m ~ matcher
  => Query strategy matcher target out
  -> strategy out
mmatchAll' tgt qu = query' @matcher @strategy qu tgt
