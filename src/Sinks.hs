module Sinks
  ( stringSink 
  , plyAsciiSink
  , plyBinarySink
  , plyTripletAsciiSink
  ) where

import Classes
import Data.Void
import Conduit
import System.IO
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BSL

--------------------------------------------------------------------------------

stringSink :: String -> ConduitT String Void IO ()
stringSink path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  go h
  liftIO $ hClose h
  where
    go h = do
      may <- await
      case may of
        Nothing -> pure ()
        Just x -> do
          liftIO $ hPutStr h x
          go h

--------------------------------------------------------------------------------

plyAsciiSink :: (X a, Y a, Z a) => String -> ConduitT a Void IO ()
plyAsciiSink path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  liftIO $ hPutStrLn h "ply"
  liftIO $ hPutStrLn h "format ascii 1.0"
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h placeholder
  liftIO $ hPutStrLn h "property float x"
  liftIO $ hPutStrLn h "property float y"
  liftIO $ hPutStrLn h "property float z"
  liftIO $ hPutStrLn h "end_header"
  go h 0 placeholderPos
  liftIO $ hClose h
  where
    go h count placeholderPos = do
      may <- await
      case may of
        Just x -> do
          liftIO $ hPutStr h (toStr x)
          go h (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ hSeek h AbsoluteSeek placeholderPos
          liftIO $ hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v     = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholder = "element vertex 0\ncomment ##################################"

--- TODO lots of duped code
plyTripletAsciiSink :: (X a, Y a, Z a) => String -> ConduitT (a, a, a) Void IO ()
plyTripletAsciiSink path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  liftIO $ hPutStrLn h "ply"
  liftIO $ hPutStrLn h "format ascii 1.0"
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h placeholderVs
  liftIO $ hPutStrLn h "property float x"
  liftIO $ hPutStrLn h "property float y"
  liftIO $ hPutStrLn h "property float z"
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h placeholderFs
  liftIO $ hPutStrLn h "property list uchar int vertex_index"
  liftIO $ hPutStrLn h "end_header"
  go h 0 placeholderVsPos placeholderFsPos
  liftIO $ hClose h
  where
    --- TODO super messy now, cleanup
    go h countFs placeholderVsPos placeholderFsPos = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ hPutStr h (toStr a)
          liftIO $ hPutStr h (toStr b)
          liftIO $ hPutStr h (toStr c)
          go h (countFs+1) placeholderVsPos placeholderFsPos
        Nothing -> do
          let placeholderVslength = length placeholderVs
              replacementVs       = "element vertex " ++ show (3 * countFs) ++ "\ncomment "
              placeholderFslength = length placeholderFs
              replacementFs       = "element face " ++ show countFs ++ "\ncomment "
          liftIO $ mapM_ (printFace h) [0..countFs-1] --- TODO error if no faces!?
          liftIO $ hSeek h AbsoluteSeek placeholderVsPos
          liftIO $ hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          liftIO $ hSeek h AbsoluteSeek placeholderFsPos
          liftIO $ hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v         = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholderVs   = "element vertex 0\ncomment ##################################"
    placeholderFs   = "element face 0\ncomment ##################################"
    printFace h fid = hPutStrLn h $ "3 " ++ (show $ 3*fid+0) ++ " " ++ (show $ 3*fid+1) ++ " " ++ (show $ 3*fid+2)
    

--------------------------------------------------------------------------------

--- TODO must switch between big/little endian properly
--- see http://hackage.haskell.org/package/cpu-0.1.2/docs/System-Endian.html
--- also consider switching between float/double http://paulbourke.net/dataformats/ply/
--- TODO lots of dupe code
plyBinarySink :: (X a, Y a, Z a) => String -> ConduitT a Void IO ()
plyBinarySink path = do
  h <- liftIO $ openBinaryFile path WriteMode --TODO consider withFile
  liftIO $ hPutStrLn h "ply"
  liftIO $ hPutStrLn h "format binary_big_endian 1.0"
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h placeholder
  liftIO $ hPutStrLn h "property float x"
  liftIO $ hPutStrLn h "property float y"
  liftIO $ hPutStrLn h "property float z"
  liftIO $ hPutStrLn h "end_header"
  go h 0 placeholderPos
  liftIO $ hClose h
  where
    go h count placeholderPos = do
      may <- await
      case may of
        Just v -> do
          ---TODO likely very inefficient, at least use 'runPut' only once for all 3
          liftIO $ BSL.hPutStr h $ float2BSL $ realToFrac $ getx $ v
          liftIO $ BSL.hPutStr h $ float2BSL $ realToFrac $ gety $ v
          liftIO $ BSL.hPutStr h $ float2BSL $ realToFrac $ getz $ v
          go h (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ hSeek h AbsoluteSeek placeholderPos
          liftIO $ hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'

    placeholder = "element vertex 0\ncomment ##################################"

--------------------------------------------------------------------------------

--- TODO is specialized for big endian, name accordingly and offer le version
--- https://hackage.haskell.org/package/binary-0.8.6.0/docs/Data-Binary-Put.html
--double2BSL :: Double -> BSL.ByteString
--double2BSL = P.runPut . P.putDoublebe

float2BSL :: Float -> BSL.ByteString
float2BSL = P.runPut . P.putFloatbe