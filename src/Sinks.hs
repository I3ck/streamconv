{-# LANGUAGE RecordWildCards #-}

module Sinks
  ( stringSink
  , objSink
  , objTripletSink
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

stringSink :: Environment -> ConduitT String Void IO ()
stringSink Environment{..} = do
  go
  where
    go = do
      may <- await
      case may of
        Nothing -> pure ()
        Just x -> do
          liftIO $ hPutStr eHandle x
          go

--------------------------------------------------------------------------------

plyAsciiSink :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO ()
plyAsciiSink Environment{..} = do
  liftIO $ hPutStr eHandle $ unlines 
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
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
          liftIO $ hPutStr eHandle (toStr x)
          go (count+1) placeholderPos
        Nothing -> liftIO $ do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          hSeek eHandle AbsoluteSeek placeholderPos
          hPutStrLn eHandle $ replacement ++ replicate (placeholderlength - length replacement) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v     = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholder = "element vertex 0\ncomment ##################################"

--------------------------------------------------------------------------------

--- TODO later require optional normals
stlAsciiSink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
stlAsciiSink Environment{..} = do
  liftIO $ hPutStrLn eHandle "solid " 
  go
  where
    go = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ do
            hPutStrLn eHandle "facet normal 0.0 0.0 0.0" --TODO later write normals
            hPutStrLn eHandle "    outer loop"
            hPutStrLn eHandle (toStr a)
            hPutStrLn eHandle (toStr b)
            hPutStrLn eHandle (toStr c)
            hPutStrLn eHandle "    endloop"
            hPutStrLn eHandle "endfacet"
          go
        Nothing -> do
          liftIO $ hPutStrLn eHandle "endsolid "
          pure ()
    toStr v = "        vertex " ++ (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) 

--- TODO later require optional normals
--- TODO possibly will require usage of unsigned integers!!!!!
stlBinarySink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
stlBinarySink Environment{..} = do
  liftIO $ mapM_ (\_ -> BSL.hPutStr eHandle $ uchar2BSL 0) [0..79] 
  placeholderPos <- liftIO $ hTell eHandle
  liftIO $ BSL.hPutStr eHandle $ int2leBSL 0
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
            BSL.hPutStr eHandle $ int16ToleBSL 0
          go placeholderPos (count+1)
        Nothing -> do
          liftIO $ hSeek eHandle AbsoluteSeek placeholderPos
          liftIO $ BSL.hPutStr eHandle $ int2leBSL count
          pure ()
    writeV v = do
      write $ realToFrac $ getx $ v
      write $ realToFrac $ gety $ v
      write $ realToFrac $ getz $ v
    write d  = BSL.hPutStr eHandle $ float2leBSL d



--- TODO lots of duped code
plyTripletAsciiSink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
plyTripletAsciiSink Environment{..} = do
  liftIO $ hPutStr eHandle $ unlines
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
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
            hPutStr eHandle (toStr a)
            hPutStr eHandle (toStr b)
            hPutStr eHandle (toStr c)
          go (countFs+1) placeholderVsPos placeholderFsPos
        Nothing -> liftIO $ do
          let placeholderVslength = length placeholderVs
              replacementVs       = "element vertex " ++ show (3 * countFs) ++ "\ncomment "
              placeholderFslength = length placeholderFs
              replacementFs       = "element face " ++ show countFs ++ "\ncomment "
          mapM_ printFace [0..countFs-1] --- TODO error if no faces!?
          hSeek eHandle AbsoluteSeek placeholderVsPos
          hPutStrLn eHandle $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek eHandle AbsoluteSeek placeholderFsPos
          hPutStrLn eHandle $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v       = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"
    printFace fid = hPutStrLn eHandle $ "3 " ++ (show $ 3*fid+0) ++ " " ++ (show $ 3*fid+1) ++ " " ++ (show $ 3*fid+2)
    
--------------------------------------------------------------------------------
---TODO rename
---TODO implement
---TODO move somewhere else since signature doesnt match?
plyAsciiSink' :: (X a, Y a, Z a) => Environment -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ()
plyAsciiSink' e@Environment{..} cv cf = do
  -- TODO lots of duped code
  liftIO $ hPutStr eHandle $ unlines
    [ "ply"
    , "format ascii 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderFs
    , "property list uchar int vertex_index"
    , "end_header"
    ]
  countVs <- runConduit $ cv .| plyOnlyVertexAscii e
  countFs <- runConduit $ cf .| plyOnlyFaceAscii e
  let placeholderVslength = length placeholderVs
      replacementVs       = "element vertex " ++ show countVs ++ "\ncomment "
      placeholderFslength = length placeholderFs
      replacementFs       = "element face " ++ show countFs ++ "\ncomment "
  hSeek eHandle AbsoluteSeek placeholderVsPos
  hPutStrLn eHandle $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
  hSeek eHandle AbsoluteSeek placeholderFsPos
  hPutStrLn eHandle $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

  where --TODO these must be helper methods elsewhere
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"

-- assumes handle at right pos
plyOnlyVertexAscii :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO Int
plyOnlyVertexAscii Environment{..} = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn eHandle $ toStr v
          go $ count+1
        Nothing -> pure count
    toStr v = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) --TODO use "showS trick"

-- assumes handle at right pos
plyOnlyFaceAscii :: Environment -> ConduitT Face Void IO Int
plyOnlyFaceAscii Environment{..} = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStr eHandle $ toStr v
          go $ count+1
        Nothing -> pure count
    toStr (Face a b c) = "3 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ "\n"

--------------------------------------------------------------------------------

--- TODO must switch between big/little endian properly
--- see http://hackage.haskell.org/package/cpu-0.1.2/docs/System-Endian.html
--- also consider switching between float/double http://paulbourke.net/dataformats/ply/
--- TODO lots of dupe code
plyBinarySink :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO ()
plyBinarySink Environment{..} = do
  liftIO $ hPutStr eHandle $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
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
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getx $ v
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ gety $ v
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getz $ v
          go (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ do 
            hSeek eHandle AbsoluteSeek placeholderPos
            hPutStrLn eHandle $ replacement ++ replicate (placeholderlength - length replacement) '#'

    placeholder = "element vertex 0\ncomment ##################################"

---TODO rename
---TODO implement
---TODO move somewhere else since signature doesnt match?
plyBinarySink' :: (X a, Y a, Z a) => Environment -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ()
plyBinarySink' e@Environment{..} cv cf = do
  -- TODO lots of duped code
  liftIO $ hPutStr eHandle $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderFs
    , "property list uchar int vertex_index"
    , "end_header"
    ]
  countVs <- runConduit $ cv .| plyOnlyVertexBinary e
  countFs <- runConduit $ cf .| plyOnlyFaceBinary e
  let placeholderVslength = length placeholderVs
      replacementVs       = "element vertex " ++ show countVs ++ "\ncomment "
      placeholderFslength = length placeholderFs
      replacementFs       = "element face " ++ show countFs ++ "\ncomment "
  hSeek eHandle AbsoluteSeek placeholderVsPos
  hPutStrLn eHandle $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
  hSeek eHandle AbsoluteSeek placeholderFsPos
  hPutStrLn eHandle $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

  where --TODO these must be helper methods elsewhere
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"

plyOnlyVertexBinary :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO Int
plyOnlyVertexBinary Environment{..} = go 0
  where
    go count = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ do 
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getx $ v
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ gety $ v
            BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getz $ v
          go $ count+1
        Nothing -> pure count

-- assumes handle at right pos
plyOnlyFaceBinary :: Environment -> ConduitT Face Void IO Int
plyOnlyFaceBinary Environment{..} = go 0
  where
    go count = do
      may <- await
      case may of
        Just (Face a b c)  -> do 
          liftIO $ do 
            BSL.hPutStr eHandle $ uchar2BSL 3
            BSL.hPutStr eHandle $ int2beBSL $ fromIntegral a
            BSL.hPutStr eHandle $ int2beBSL $ fromIntegral b
            BSL.hPutStr eHandle $ int2beBSL $ fromIntegral c
          go $ count+1
        Nothing -> pure count
    
plyTripletBinarySink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
plyTripletBinarySink Environment{..} = do
  liftIO $ hPutStr eHandle $ unlines
    [ "ply"
    , "format binary_big_endian 1.0"
    ]
  placeholderVsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
    [ placeholderVs
    , "property float x"
    , "property float y"
    , "property float z"
    ]
  placeholderFsPos <- liftIO $ hTell eHandle
  liftIO $ hPutStr eHandle $ unlines
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
          hSeek eHandle AbsoluteSeek placeholderVsPos
          hPutStrLn eHandle $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek eHandle AbsoluteSeek placeholderFsPos
          hPutStrLn eHandle $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"
    writeFace fid = do
      BSL.hPutStr eHandle $ uchar2BSL 3
      BSL.hPutStr eHandle $ int2beBSL (3*fid+0)
      BSL.hPutStr eHandle $ int2beBSL (3*fid+1)
      BSL.hPutStr eHandle $ int2beBSL (3*fid+2)
    writeVertex v   = do
      BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getx $ v
      BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ gety $ v
      BSL.hPutStr eHandle $ float2beBSL $ realToFrac $ getz $ v

--------------------------------------------------------------------------------

objSink :: (X a, Y a, Z a) => Environment -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ()
objSink e cv cf = do
  runConduit $ cv .| objOnlyVertex e
  runConduit $ cf .| objOnlyFace e

objOnlyVertex :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO ()
objOnlyVertex Environment{..} = go
  where
    go = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn eHandle $ toStr v
          go
        Nothing -> pure ()
    toStr v = "v " ++ (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) --TODO use "showS trick"

-- assumes handle at right pos
objOnlyFace :: Environment -> ConduitT Face Void IO ()
objOnlyFace Environment{..} = go
  where
    go = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn eHandle $ toStr v
          go
        Nothing -> pure ()
    toStr (Face a b c) = "f " ++ show (a+1) ++ " " ++ show (b+1) ++ " " ++ show (c+1)

objTripletSink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
objTripletSink Environment{..} = go 0
  where
    go countFs = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ do 
            hPutStr eHandle (toStr a)
            hPutStr eHandle (toStr b)
            hPutStr eHandle (toStr c)
          go (countFs+1)
        Nothing -> liftIO $ mapM_ printFace [0..countFs-1] --- TODO error if no faces!?

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v       = "v " ++ (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"
    printFace fid = hPutStrLn eHandle $ "f " ++ (show $ 3*fid+1) ++ " " ++ (show $ 3*fid+2) ++ " " ++ (show $ 3*fid+3)

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