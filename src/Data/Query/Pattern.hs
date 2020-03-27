{-# LANGUAGE PatternSynonyms #-}

module Data.Query.Pattern
  ( Pattern
  , ReturnList(Nil, Cons, (:-))
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )


data ReturnList (tags :: [a]) (tgts :: [Type]) where
  Nil ::ReturnList '[] '[]
  Cons :: Proxy tag -> tgt -> ReturnList tags tgts -> ReturnList (tag ': tags) (tgt ': tgts)

pattern (:-) :: tgt -> ReturnList tags tgts -> ReturnList (tag ': tags) (tgt ': tgts)
pattern (:-) x xs = Cons Proxy x xs

infixr 2 :-
{-# COMPLETE (:-) #-}

type Pattern tag tgt tagOut tgtOut
  = forall m . MonadSearch m => Proxy tag -> tgt -> m (ReturnList tagOut tgtOut)
