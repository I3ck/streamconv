{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Types
import Instances ()
import Sources
import Transformers
import Sinks

--------------------------------------------------------------------------------

main :: IO ()
main = do
  runConduit $ (yieldMany . replicate 10000000 $ Position 3 4 5) .| xyzToStrC " " "\n" .| stringSink "output1.tmp"
  runConduit $ (yieldMany . replicate 1000 $ Position 3 4 5) .| xyToStrC  ";" "\n" .| stringSink "output2.tmp"
  runConduit $ xyz "output1.tmp" " " "\n" .| xyToStrC  ";" "\n" .| stringSink "output3.tmp"
  pure ()
