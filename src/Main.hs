{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Types
import Instances ()
import Sources
import Transformers
import Sinks
import System.IO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

--------------------------------------------------------------------------------

bufferSize = 100

--- TODO expand to support all possible combinations (for now, later consider better abstraction to not have quadratic complexity)
run :: String -> String -> Format -> Format -> IO ()
run pf pt = run'
  where
    run' :: Format -> Format -> IO ()
    run' StlAscii Obj 
      = withBlobHandle (\ b h -> runConduit $ stl b .| untriple .| objToStr bufferSize .| stringSink h)

    run' StlAscii StlBinary
      = withBlobHandle (\b h -> runConduit $ stl b .| stlBinarySink h)

    run' StlAscii PlyAscii 
      = withBlobHandle (\b h -> runConduit $ stl b .| plyTripletAsciiSink h)

    run' StlAscii PlyBinary
      = withBlobHandle (\b h -> runConduit $ stl b .| plyTripletBinarySink h)

    run' Obj PlyAscii
      = withFile pt WriteMode (\h -> do
        (cv, cf) <- obj pf
        plyAsciiSink' h cv cf)

    run' Obj PlyBinary
      = withFile pt WriteMode (\h -> do
        (cv, cf) <- obj pf
        plyBinarySink' h cv cf)

{- TODO implement
    run' Obj PlyBin
      = withFile pt WriteMode (\h -> do
        (cv, cf) <- obj pf
        plyBinSink' h cv cf)
-}

    
    run' _ _ = putStrLn "Conversion not supported" ---TODO more info

    withBlobHandle :: (L.Text -> Handle -> IO ()) -> IO ()
    withBlobHandle f = withFile pt WriteMode (\h -> do
                       blob <- LIO.readFile pf
                       f blob h)

main :: IO ()
main = do
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
  withFile "tmp/outputstlstl.obj" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| untriple
      .| objToStr bufferSize
      .| stringSink h)

  withFile "tmp/outputstl.ply" WriteMode (\h -> do
    blob <- (LIO.readFile "tmp/stlascii.stl") 
    runConduit $
         stl blob
      .| plyTripletAsciiSink h)

  withFile "tmp/outputstlBIN.ply" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| plyTripletBinarySink h)

  withFile "tmp/outputstlSTLASCII.stl" WriteMode (\h -> do 
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| stlAsciiSink h)

  withFile "tmp/outputstlSTLbinary.stl" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob     
      .| stlBinarySink h)

  -- CURRENTLY FAILS, SEE TODO
  withFile "tmp/outputstlAgain.ply" WriteMode (\h -> do
    (cv, cf) <- ply "tmp/outputstl.ply"
    plyAsciiSink' h cv cf
    )

  withFile "tmp/objFile.ply" WriteMode (\h -> do
    (cv, cf) <- obj "tmp/obj.obj"
    plyAsciiSink' h cv cf
    )

  withFile "tmp/objFileToBin.ply" WriteMode (\h -> do
    (cv, cf) <- obj "tmp/obj.obj"
    plyBinarySink' h cv cf
    )
    
  pure ()
