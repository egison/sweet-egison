{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Egison.Match
  ( matchAll
  , match
  , matchAllDFS
  , matchDFS
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..)
                                                , BFS
                                                , DFS
                                                )
import           Data.Query                     ( Query
                                                , query
                                                )


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
matchAll tgt _ q = collect $ query @matcher (mconcat q) tgt

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
matchAllDFS tgt _ q = collect $ query @matcher (mconcat q) tgt
