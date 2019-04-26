{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
--import Types
import Instances ()
import Sources
import Transformers
import Sinks
import System.IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let bufferSize = 100
{-
  runConduit $ 
       (yieldMany $ fmap (\i -> Position i (2*i) (3*i)) [0..10000000-1]) 
    .| xyzToStr bufferSize " " "\n" 
    .| stringSink "tmp/output1.xyz"
{-
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
-}
  runConduit $
       xyz "tmp/output1.xyz" " " "\n"
    .| plyBinarySink "tmp/outputBin.ply"
-}
  withFile "tmp/outputstlstl.obj" WriteMode (\h -> runConduit $
       stl "tmp/stlascii.stl"
    .| untriple
    .| objToStr bufferSize
    .| stringSink h)

  withFile "tmp/outputstl.ply" WriteMode (\h -> runConduit $
       stl "tmp/stlascii.stl"
    .| plyTripletAsciiSink h)

  withFile "tmp/outputstlBIN.ply" WriteMode (\h -> runConduit $
       stl "tmp/stlascii.stl"
    .| plyTripletBinarySink h)

  withFile "tmp/outputstlSTLASCII.stl" WriteMode (\h -> runConduit $
       stl "tmp/stlascii.stl"
    .| stlAsciiSink h)
    
  pure ()
