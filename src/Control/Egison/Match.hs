module Control.Egison.Match
  ( MatchClause
  , matchAll
  , match
  , with
  )
where

import           Control.Monad.Search           ( MonadSearch(..) )


type MatchClause strategy tgt out = tgt -> strategy out

match :: forall strategy out . MonadSearch strategy => strategy out -> out
match = head . matchAll

matchAll :: forall strategy out . MonadSearch strategy => strategy out -> [out]
matchAll = collect

with :: tgt -> MatchClause strategy tgt out -> strategy out
with x mc = mc x

infix 1 `with`
