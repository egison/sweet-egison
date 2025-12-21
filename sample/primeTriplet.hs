{-# LANGUAGE QuasiQuotes #-}

import Control.Egison
import Data.Numbers.Primes (primes)

main :: IO ()
main = print $ take 10 results
  where
    results =
      matchAll
        bfs
        primes
        (List Eql)
        [ [mc| _ ++ $x : ($y & (#(x + 2) | #(x + 4))) : #(x + 6) : _ -> (x, y, x + 6) |]
        ]
