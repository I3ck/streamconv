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

stringSink :: Handle -> ConduitT String Void IO ()
stringSink h = do
  go
  where
    go = do
      may <- await
      case may of
        Nothing -> pure ()
        Just x -> do
          liftIO $ hPutStr h x
          go

--------------------------------------------------------------------------------

plyAsciiSink :: (X a, Y a, Z a) => Handle -> ConduitT a Void IO ()
plyAsciiSink h = do
  liftIO $ hPutStrLn h $ unlines 
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h $ unlines
    [ placeholder
    , "property float x"
    , "property float y"
    , "property float z"
    , "end_header"
    ]
  go 0 placeholderPos
  where
    go count placeholderPos = do
      may <- await
      case may of
        Just x -> do
          liftIO $ hPutStr h (toStr x)
          go (count+1) placeholderPos
        Nothing -> liftIO $ do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          hSeek h AbsoluteSeek placeholderPos
          hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v     = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholder = "element vertex 0\ncomment ##################################"

--- TODO lots of duped code
plyTripletAsciiSink :: (X a, Y a, Z a) => Handle -> ConduitT (a, a, a) Void IO ()
plyTripletAsciiSink h = do
  liftIO $ hPutStrLn h $ unlines
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h $ unlines
    [ placeholderFs
    , "property list uchar int vertex_index"
    , "end_header"
    ]
  go 0 placeholderVsPos placeholderFsPos
  where
    --- TODO super messy now, cleanup
    go countFs placeholderVsPos placeholderFsPos = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ do 
            hPutStr h (toStr a)
            hPutStr h (toStr b)
            hPutStr h (toStr c)
          go (countFs+1) placeholderVsPos placeholderFsPos
        Nothing -> liftIO $ do
          let placeholderVslength = length placeholderVs
              replacementVs       = "element vertex " ++ show (3 * countFs) ++ "\ncomment "
              placeholderFslength = length placeholderFs
              replacementFs       = "element face " ++ show countFs ++ "\ncomment "
          mapM_ (printFace h) [0..countFs-1] --- TODO error if no faces!?
          hSeek h AbsoluteSeek placeholderVsPos
          hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek h AbsoluteSeek placeholderFsPos
          hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

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
plyBinarySink :: (X a, Y a, Z a) => Handle -> ConduitT a Void IO ()
plyBinarySink h = do
  liftIO $ hPutStrLn h $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h $ unlines
    [ placeholder
    , "property float x"
    , "property float y"
    , "property float z"
    , "end_header"
    ]
  go 0 placeholderPos
  where
    go count placeholderPos = do
      may <- await
      case may of
        Just v -> do
          ---TODO likely very inefficient, at least use 'runPut' only once for all 3
          liftIO $ do 
            BSL.hPutStr h $ float2BSL $ realToFrac $ getx $ v
            BSL.hPutStr h $ float2BSL $ realToFrac $ gety $ v
            BSL.hPutStr h $ float2BSL $ realToFrac $ getz $ v
          go (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ do 
            hSeek h AbsoluteSeek placeholderPos
            hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'

    placeholder = "element vertex 0\ncomment ##################################"

--------------------------------------------------------------------------------

--- TODO is specialized for big endian, name accordingly and offer le version
--- https://hackage.haskell.org/package/binary-0.8.6.0/docs/Data-Binary-Put.html
--double2BSL :: Double -> BSL.ByteString
--double2BSL = P.runPut . P.putDoublebe

float2BSL :: Float -> BSL.ByteString
float2BSL = P.runPut . P.putFloatbe