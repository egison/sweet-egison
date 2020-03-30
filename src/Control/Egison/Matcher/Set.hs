{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Egison.Matcher.Set
  ( Set(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Data.Query.Pattern.Tuple       ( tuple2 )
import           Control.Egison.Matcher         ( Matcher )
import           Control.Egison.Matcher.List    ( List(..) )
import           Control.Egison.Matcher.Pair    ( Pair )
import           Control.Egison.Matcher.Multiset
                                                ( Multiset )
import           Control.Egison.Match           ( match'
                                                , matchAll'
                                                , mc
                                                )


newtype Set m = Set m

instance Matcher m tgt => Matcher (Set m) [tgt]

instance Matcher m tgt => CollectionPattern (Set m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Set m) = m
  {-# INLINABLE nil #-}
  nil _ [] = pure ()
  nil _ _  = mzero
  {-# INLINABLE cons #-}
  cons _ xs = foldr go mzero xs where go x acc = pure (x, xs) `mplus` acc
-- TODO: Implement
  join   = undefined
  spread = undefined

instance (Matcher m tgt, ValuePattern m tgt) => ValuePattern (Set m) [tgt] where
  value a _ b =
    match' (vnub a, vnub b) @(Pair (List m) (Multiset m))
      $  [mc| ([], []) -> pure () |]
      <> [mc| ($x : $xs, (#x : #xs)) -> pure () |]
      <> [mc| _ -> mzero |]
   where
    vnub xs = matchAll' xs @(List m) [mc| _ ++ $x : !(_ ++ #x : _) -> x |]
