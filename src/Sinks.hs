module Sinks
  ( stringSink 
  , plyAsciiSink
  , plyAsciiSink'
  , plyBinarySink
  , plyBinarySink'
  , plyTripletAsciiSink
  , plyTripletBinarySink
  , stlAsciiSink
  , stlBinarySink
  ) where

import Types
import Classes
import Data.Void
import Conduit
import System.IO
import Data.Int
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
  liftIO $ hPutStr h $ unlines 
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
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

--------------------------------------------------------------------------------

--- TODO later require optional normals
stlAsciiSink :: (X a, Y a, Z a) => Handle -> ConduitT (a, a, a) Void IO ()
stlAsciiSink h = do
  liftIO $ hPutStrLn h "solid " 
  go
  where
    go = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ do
            hPutStrLn h "facet normal 0.0 0.0 0.0" --TODO later write normals
            hPutStrLn h "    outer loop"
            hPutStrLn h (toStr a)
            hPutStrLn h (toStr b)
            hPutStrLn h (toStr c)
            hPutStrLn h "    endloop"
            hPutStrLn h "endfacet"
          go
        Nothing -> do
          liftIO $ hPutStrLn h "endsolid "
          pure ()
    toStr v = "        vertex " ++ (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) 

--- TODO later require optional normals
--- TODO possibly will require usage of unsigned integers!!!!!
stlBinarySink :: (X a, Y a, Z a) => Handle -> ConduitT (a, a, a) Void IO ()
stlBinarySink h = do
  liftIO $ mapM_ (\_ -> BSL.hPutStr h $ uchar2BSL 0) [0..79] 
  placeholderPos <- liftIO $ hTell h
  liftIO $ BSL.hPutStr h $ int2leBSL 0
  go placeholderPos 0
  where
    go placeholderPos count = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ do
            write 0.0 --TODO later write normal data
            write 0.0
            write 0.0
            writeV a
            writeV b
            writeV c
            BSL.hPutStr h $ int16ToleBSL 0
          go placeholderPos (count+1)
        Nothing -> do
          liftIO $ hSeek h AbsoluteSeek placeholderPos
          liftIO $ BSL.hPutStr h $ int2leBSL count
          pure ()
    writeV v = do
      write $ realToFrac $ getx $ v
      write $ realToFrac $ gety $ v
      write $ realToFrac $ getz $ v
    write d  = BSL.hPutStr h $ float2leBSL d



--- TODO lots of duped code
plyTripletAsciiSink :: (X a, Y a, Z a) => Handle -> ConduitT (a, a, a) Void IO ()
plyTripletAsciiSink h = do
  liftIO $ hPutStr h $ unlines
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
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
          mapM_ printFace [0..countFs-1] --- TODO error if no faces!?
          hSeek h AbsoluteSeek placeholderVsPos
          hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek h AbsoluteSeek placeholderFsPos
          hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v       = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"
    printFace fid = hPutStrLn h $ "3 " ++ (show $ 3*fid+0) ++ " " ++ (show $ 3*fid+1) ++ " " ++ (show $ 3*fid+2)
    
--------------------------------------------------------------------------------
---TODO rename
---TODO implement
---TODO move somewhere else since signature doesnt match?
plyAsciiSink' :: (X a, Y a, Z a) => Handle -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ()
plyAsciiSink' h cv cf = do
  -- TODO lots of duped code
  liftIO $ hPutStr h $ unlines
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderFs
    , "property list uchar int vertex_index"
    , "end_header"
    ]
  countVs <- runConduit $ cv .| plyOnlyVertexAscii h
  countFs <- runConduit $ cf .| plyOnlyFaceAscii h
  let placeholderVslength = length placeholderVs
      replacementVs       = "element vertex " ++ show countVs ++ "\ncomment "
      placeholderFslength = length placeholderFs
      replacementFs       = "element face " ++ show countFs ++ "\ncomment "
  hSeek h AbsoluteSeek placeholderVsPos
  hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
  hSeek h AbsoluteSeek placeholderFsPos
  hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

  where --TODO these must be helper methods elsewhere
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"

-- assumes handle at right pos
plyOnlyVertexAscii :: (X a, Y a, Z a) => Handle -> ConduitT a Void IO Int
plyOnlyVertexAscii h = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn h $ toStr v
          go $ count+1
        Nothing -> pure count
    toStr v = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) --TODO use "showS trick"

-- assumes handle at right pos
plyOnlyFaceAscii :: Handle -> ConduitT Face Void IO Int
plyOnlyFaceAscii h = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStr h $ toStr v
          go $ count+1
        Nothing -> pure count
    toStr (Face a b c) = "3 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ "\n"

--------------------------------------------------------------------------------

--- TODO must switch between big/little endian properly
--- see http://hackage.haskell.org/package/cpu-0.1.2/docs/System-Endian.html
--- also consider switching between float/double http://paulbourke.net/dataformats/ply/
--- TODO lots of dupe code
plyBinarySink :: (X a, Y a, Z a) => Handle -> ConduitT a Void IO ()
plyBinarySink h = do
  liftIO $ hPutStr h $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
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
            BSL.hPutStr h $ float2beBSL $ realToFrac $ getx $ v
            BSL.hPutStr h $ float2beBSL $ realToFrac $ gety $ v
            BSL.hPutStr h $ float2beBSL $ realToFrac $ getz $ v
          go (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ do 
            hSeek h AbsoluteSeek placeholderPos
            hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'

    placeholder = "element vertex 0\ncomment ##################################"

---TODO rename
---TODO implement
---TODO move somewhere else since signature doesnt match?
plyBinarySink' :: (X a, Y a, Z a) => Handle -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ()
plyBinarySink' h cv cf = do
  -- TODO lots of duped code
  liftIO $ hPutStr h $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderFs
    , "property list uchar int vertex_index"
    , "end_header"
    ]
  countVs <- runConduit $ cv .| plyOnlyVertexBinary h
  countFs <- runConduit $ cf .| plyOnlyFaceBinary h
  let placeholderVslength = length placeholderVs
      replacementVs       = "element vertex " ++ show countVs ++ "\ncomment "
      placeholderFslength = length placeholderFs
      replacementFs       = "element face " ++ show countFs ++ "\ncomment "
  hSeek h AbsoluteSeek placeholderVsPos
  hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
  hSeek h AbsoluteSeek placeholderFsPos
  hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

  where --TODO these must be helper methods elsewhere
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"

plyOnlyVertexBinary :: (X a, Y a, Z a) => Handle -> ConduitT a Void IO Int
plyOnlyVertexBinary h = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ do 
            BSL.hPutStr h $ float2beBSL $ realToFrac $ getx $ v
            BSL.hPutStr h $ float2beBSL $ realToFrac $ gety $ v
            BSL.hPutStr h $ float2beBSL $ realToFrac $ getz $ v
          go $ count+1
        Nothing -> pure count

-- assumes handle at right pos
plyOnlyFaceBinary :: Handle -> ConduitT Face Void IO Int
plyOnlyFaceBinary h = go 0
  where
    go count = do
      may <- await
      case may of
        Just (Face a b c)  -> do 
          liftIO $ do 
            BSL.hPutStr h $ uchar2BSL 3
            BSL.hPutStr h $ int2beBSL $ fromIntegral a
            BSL.hPutStr h $ int2beBSL $ fromIntegral b
            BSL.hPutStr h $ int2beBSL $ fromIntegral c
          go $ count+1
        Nothing -> pure count
    
plyTripletBinarySink :: (X a, Y a, Z a) => Handle -> ConduitT (a, a, a) Void IO ()
plyTripletBinarySink h = do
  liftIO $ hPutStr h $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell h
  liftIO $ hPutStr h $ unlines
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
            writeVertex a
            writeVertex b
            writeVertex c
          go (countFs+1) placeholderVsPos placeholderFsPos
        Nothing -> liftIO $ do
          let placeholderVslength = length placeholderVs
              replacementVs       = "element vertex " ++ show (3 * countFs) ++ "\ncomment "
              placeholderFslength = length placeholderFs
              replacementFs       = "element face " ++ show countFs ++ "\ncomment "
          mapM_ writeFace [0..countFs-1] --- TODO error if no faces!?
          hSeek h AbsoluteSeek placeholderVsPos
          hPutStrLn h $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek h AbsoluteSeek placeholderFsPos
          hPutStrLn h $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"
    writeFace fid = do
      BSL.hPutStr h $ uchar2BSL 3
      BSL.hPutStr h $ int2beBSL (3*fid+0)
      BSL.hPutStr h $ int2beBSL (3*fid+1)
      BSL.hPutStr h $ int2beBSL (3*fid+2)
    writeVertex v   = do
      BSL.hPutStr h $ float2beBSL $ realToFrac $ getx $ v
      BSL.hPutStr h $ float2beBSL $ realToFrac $ gety $ v
      BSL.hPutStr h $ float2beBSL $ realToFrac $ getz $ v

--------------------------------------------------------------------------------

--- TODO is specialized for big endian, name accordingly and offer le version
--- https://hackage.haskell.org/package/binary-0.8.6.0/docs/Data-Binary-Put.html
--double2BSL :: Double -> BSL.ByteString
--double2BSL = P.runPut . P.putDoublebe

---TODO precision in method names
float2beBSL :: Float -> BSL.ByteString
float2beBSL = P.runPut . P.putFloatbe

float2leBSL :: Float -> BSL.ByteString
float2leBSL = P.runPut . P.putFloatle

uchar2BSL :: Int8 -> BSL.ByteString
uchar2BSL = P.runPut . P.putInt8

int2beBSL :: Int32 -> BSL.ByteString
int2beBSL = P.runPut . P.putInt32be

int2leBSL :: Int32 -> BSL.ByteString
int2leBSL = P.runPut . P.putInt32le

int16ToleBSL :: Int16 -> BSL.ByteString
int16ToleBSL = P.runPut . P.putInt16le