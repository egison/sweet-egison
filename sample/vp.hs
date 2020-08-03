{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

import           Control.Egison

import           Data.List                      ( sortBy
                                                , union
                                                )
import           Control.Monad                  ( MonadPlus(..) )

data MyInteger = MyInteger
instance Matcher MyInteger Integer

plus10
  :: Pattern
       (PP Integer)
       MyInteger
       Integer
       Integer
plus10 (VP v) _ t = if v + 10 == t then pure v else mzero
plus10 _ _ t = pure (t - 10)

plus10M MyInteger _ = Eql

vpTest n =
  match dfs n MyInteger
    [[mc| plus10 #3 -> 100 |],
     [mc| plus10 $x -> x |],
     [mc| _ -> 0 |]]

main = do
  print $ show $ vpTest 13 -- return 100
  print $ show $ vpTest 14 -- return 4
