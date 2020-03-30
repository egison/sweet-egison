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
match
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query BFS matcher target out]
  -> out
match tgt m = head . matchAll tgt m

{-# INLINABLE matchAll #-}
matchAll
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query BFS matcher target out]
  -> [out]
matchAll tgt _ qs = collect $ query @matcher (mconcat qs) tgt

{-# INLINABLE matchDFS #-}
matchDFS
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query DFS matcher target out]
  -> out
matchDFS tgt m = head . matchAllDFS tgt m

{-# INLINABLE matchAllDFS #-}
matchAllDFS
  :: forall matcher target out
   . Matcher matcher target
  => target
  -> matcher
  -> [Query DFS matcher target out]
  -> [out]
matchAllDFS tgt _ qs = collect $ query @matcher (mconcat qs) tgt

-- | 'QuasiQuoter' for match clauses, an alias of 'q'
mc :: QuasiQuoter
mc = q


{-# INLINABLE match' #-}
match'
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> out
match' tgt = head . matchAll' tgt

{-# INLINABLE matchAll' #-}
matchAll'
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> [out]
matchAll' tgt qu = collect $ query @matcher qu tgt

{-# INLINABLE mmatchAll' #-}
mmatchAll'
  :: forall strategy matcher target out
   . (Matcher matcher target, MonadSearch strategy)
  => target
  -> forall m
   . m ~ matcher
  => Query strategy matcher target out
  -> strategy out
mmatchAll' tgt qu = query' @matcher @strategy qu tgt
