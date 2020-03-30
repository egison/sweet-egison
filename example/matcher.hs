{-# LANGUAGE QuasiQuotes #-}

import           Control.Egison


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
