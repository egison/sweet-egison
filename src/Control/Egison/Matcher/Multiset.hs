{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Egison.Matcher.Multiset
  ( Multiset(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Data.Query.Pattern.Tuple       ( tuple2 )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Egison.Matcher.List    ( List(..) )
import           Control.Egison.Matcher.Pair    ( Pair )
import           Control.Egison.Match           ( match'
                                                , mmatchAll'
                                                , mc
                                                )


newtype Multiset m = Multiset m

instance Matcher m tgt => Matcher (Multiset m) [tgt]

instance Matcher m tgt => CollectionPattern (Multiset m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Multiset m) = m
  {-# INLINABLE nil #-}
  nil _ [] = pure ()
  nil _ _  = mzero
  {-# INLINABLE cons #-}
  cons _ xs = mmatchAll' xs @(List m) [mc| $hs ++ $x : $ts -> (x, hs ++ ts) |]
  -- TODO: Implement
  join   = undefined
  spread = undefined

instance (Matcher m tgt, ValuePattern m tgt) => ValuePattern (Multiset m) [tgt] where
  value a _ b =
    match' (a, b) @(Pair (List m) (Multiset m))
      $  [mc| ([], []) -> pure () |]
      <> [mc| ($x : $xs, (#x : #xs)) -> pure () |]
      <> [mc| _ -> mzero |]
