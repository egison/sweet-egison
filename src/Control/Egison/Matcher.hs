{-# LANGUAGE ConstraintKinds #-}
module Control.Egison.Matcher
  ( Matcher
  , Something(..)
  , EqM(..)
  , IntegralM(..)
  , Eql
  , Integer
  )
where

import           Prelude                 hiding ( Integer )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Data.Tagged                    ( Tagged(..) )


class Matcher m tgt


data Something = Something

instance Matcher Something a


data EqM = EqM

instance Eq a => Matcher EqM a

instance Eq a => ValuePattern EqM a where
  value (Tagged a) b | a == b    = pure ()
                     | otherwise = mzero

type Eql = EqM

data IntegralM = IntegralM

instance Integral a => Matcher IntegralM a

instance Integral a => ValuePattern IntegralM a where
  value (Tagged a) b | a == b    = pure ()
                     | otherwise = mzero

type Integer = IntegralM
