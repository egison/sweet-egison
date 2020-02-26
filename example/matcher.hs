{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Control.Egison.Matcher.Set


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll @BFS $ [1 ..] `as` Set @(Plain Int) `with` [search|
      $x : $y : _ => (x, y)
    |]
