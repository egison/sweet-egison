{-# LANGUAGE QuasiQuotes #-}

import           Control.Egison

import           Data.Numbers.Primes            ( primes )


main :: IO ()
main = print $ results
 where
--  results = matchAll dfs 2 Something [[mc| _ -> 1 |]]
--  results = matchAll dfs [1, 2, 3] (List Something) [[mc| $x : $xs -> (x, xs) |]]
  results = let x = (2 :: Integer) in matchAll dfs x Eql [[mc| #x -> 1 |]]
