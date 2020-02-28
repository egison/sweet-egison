module Control.Egison.Match
  ( matchAll
  , match
  , with
  )
where

import           Control.Monad.Search           ( MonadSearch(..) )
import           Data.Query                     ( Query
                                                , find
                                                )


match :: forall strategy out . MonadSearch strategy => strategy out -> out
match = head . matchAll

matchAll :: forall strategy out . MonadSearch strategy => strategy out -> [out]
matchAll = collect

with :: MonadSearch strategy => tgt -> Query strategy tgt out -> strategy out
with x q = find q x

infix 1 `with`
