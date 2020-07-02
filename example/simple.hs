{-# LANGUAGE QuasiQuotes #-}

import           Control.Egison

import           Data.Numbers.Primes            ( primes )


main :: IO ()
main = print $ results
 where
--  results = matchAll dfs 2 Something [[mc| _ -> 1 |]]
  results = matchAll dfs [1, 2, 3] (List Something) [[mc| cons $x $xs -> (x, xs) |]]
