{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Data.Numbers.Primes            ( primes )


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll @BFS $ primes `with` [search|
      (tails (cons $x (cons ($y & (#(x + 2) | #(x + 4))) (cons #(x + 6) _)))) => (x, y, x + 6)
    |]
