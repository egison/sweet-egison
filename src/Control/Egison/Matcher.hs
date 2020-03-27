module Control.Egison.Matcher
  ( Matcher
  , Something(..)
  , Eql(..)
  , Integer(..)
  )
where

import           Prelude                 hiding ( Integer )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Value       ( ValuePattern(..) )
import           Data.Tagged                    ( Tagged(..) )


class Matcher m tgt


data Something = Something

instance Matcher Something a


data Eql = Eql

instance Eq a => Matcher Eql a

instance Eq a => ValuePattern Eql a where
  value (Tagged a) b | a == b    = pure ()
                     | otherwise = mzero


data Integer = Integer

instance Integral a => Matcher Integer a

instance Integral a => ValuePattern Integer a where
  value (Tagged a) b | a == b    = pure ()
                     | otherwise = mzero
