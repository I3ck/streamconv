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
  let bufferSize = 100

  runConduit $ 
       (yieldMany . replicate 10000000 $ Position 3 4 5) 
    .| xyzToStr bufferSize " " "\n" 
    .| stringSink "tmp/output1.xyz"

  runConduit $ 
       (yieldMany . replicate 1000 $ Position 3 4 5) 
    .| xyToStr bufferSize ";" "\n" 
    .| stringSink "tmp/output2.xy"
  
  runConduit $ 
       xyz "tmp/output1.xyz" " " "\n" 
    .| xyToStr bufferSize  ";" "\n" 
    .| stringSink "tmp/output3.xyz"

  runConduit $ 
       xyz "tmp/output1.xyz" " " "\n" 
    .| objToStr bufferSize 
    .| stringSink "tmp/output.obj"

  runConduit $
       xyz "tmp/output1.xyz" " " "\n"
    .| plyAsciiSink "tmp/output.ply"
    
  pure ()
