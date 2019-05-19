{-# LANGUAGE RecordWildCards #-}

module Sinks
  ( xyzSinks
  , xySinks
  , tripletSinks
  , faceSinks
  ) where

import Types
import Classes
import Utils
import Data.Void
import Conduit
import System.IO
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BL

--------------------------------------------------------------------------------

xySinks :: (X a, Y a) => M.Map Format (Environment -> ConduitT a Void IO ())
xySinks = M.fromList
  [ (Xy, xySink)
  ]

xyzSinks :: (X a, Y a, Z a) => M.Map Format (Environment -> ConduitT a Void IO ())
xyzSinks = M.fromList
  [ (PlyAscii,  plyAsciiSink)
  , (PlyBinary, plyBinarySink)
  , (Obj,       objOnlyVertex)
  , (Xyz,       xyzSink)
  ]

tripletSinks :: (X a, Y a, Z a) => M.Map Format (Environment -> ConduitT (a, a, a) Void IO ())
tripletSinks = M.fromList
  [ (StlAscii,  stlAsciiSink)
  , (StlBinary, stlBinarySink)
  , (PlyAscii,  plyTripletAsciiSink)
  , (PlyBinary, plyTripletBinarySink)
  , (Obj,       objTripletSink)
  ]

faceSinks :: (X a, Y a, Z a) => M.Map Format (Environment -> ConduitT () a IO () -> ConduitT () Face IO () -> IO ())
faceSinks = M.fromList
  [ (PlyAscii,  plyAsciiSink')
  , (PlyBinary, plyBinarySink')
  , (Obj,       objSink)
  ]

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
          liftIO $ hPutStrLn eHandle $ unlines
            [ toStrNormal a b c
            , "        outer loop"
            , toStr a
            , toStr b
            , toStr c
            , "        endloop"
            , "    endfacet"
            ]
          go
        Nothing -> do
          liftIO $ hPutStrLn eHandle "endsolid "
          pure ()
    toStr v = "            vertex " ++ (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) 
    toStrNormal a b c = "    facet normal " ++ (show . i $ n) ++ " " ++ (show . j $ n) ++ " " ++ (show . k $ n)
      where
        n = faceNormal (toPos a) (toPos b) (toPos c)

--- TODO later require optional normals
--- TODO possibly will require usage of unsigned integers!!!!!
stlBinarySink :: (X a, Y a, Z a) => Environment -> ConduitT (a, a, a) Void IO ()
stlBinarySink Environment{..} = do
  liftIO $ BL.hPutStr eHandle $ P.runPut $ mapM_ (\_ -> P.putInt8 0) [0..79]
  placeholderPos <- liftIO $ hTell eHandle
  liftIO $ BL.hPutStr eHandle $ P.runPut $ P.putInt32le 0
  go placeholderPos 0
  where
    go placeholderPos count = do
      may <- await
      case may of
        Just (a, b, c) -> do
          liftIO $ BL.hPutStr eHandle $ P.runPut $ do
            writeN a b c
            writeV a
            writeV b
            writeV c
            P.putInt16le 0
          go placeholderPos (count+1)
        Nothing -> do
          liftIO $ hSeek eHandle AbsoluteSeek placeholderPos
          liftIO $ BL.hPutStr eHandle $ P.runPut $ P.putInt32le count
          pure ()
    writeV v = do
      write $ getx v
      write $ gety v
      write $ getz v
    writeN a b c = do
      write $ i n
      write $ j n
      write $ k n
      where
        n = faceNormal (toPos a) (toPos b) (toPos c)
    write d  = P.putFloatle $ realToFrac d

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
          liftIO $ BL.hPutStr eHandle $ P.runPut $ do
            P.putFloatbe $ realToFrac $ getx v
            P.putFloatbe $ realToFrac $ gety v
            P.putFloatbe $ realToFrac $ getz v
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
          liftIO $ BL.hPutStr eHandle $ P.runPut $ do
            P.putFloatbe $ realToFrac $ getx v
            P.putFloatbe $ realToFrac $ gety v
            P.putFloatbe $ realToFrac $ getz v
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
          liftIO $ BL.hPutStr eHandle $ P.runPut $ do 
            P.putInt8 3
            P.putInt32be $ fromIntegral a
            P.putInt32be $ fromIntegral b
            P.putInt32be $ fromIntegral c
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
          liftIO $ BL.hPutStr eHandle $ P.runPut $ do 
            writeVertex a
            writeVertex b
            writeVertex c
          go (countFs+1) placeholderVsPos placeholderFsPos
        Nothing -> liftIO $ do
          let placeholderVslength = length placeholderVs
              replacementVs       = "element vertex " ++ show (3 * countFs) ++ "\ncomment "
              placeholderFslength = length placeholderFs
              replacementFs       = "element face " ++ show countFs ++ "\ncomment "
          BL.hPutStr eHandle $ P.runPut $ mapM_ writeFace [0..countFs-1] --- TODO error if no faces!?
          hSeek eHandle AbsoluteSeek placeholderVsPos
          hPutStrLn eHandle $ replacementVs ++ replicate (placeholderVslength - length replacementVs) '#'
          hSeek eHandle AbsoluteSeek placeholderFsPos
          hPutStrLn eHandle $ replacementFs ++ replicate (placeholderFslength - length replacementFs) '#'

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    placeholderVs = "element vertex 0\ncomment ##################################"
    placeholderFs = "element face 0\ncomment ##################################"
    writeFace fid = do
      P.putInt8 3
      P.putInt32be $ 3*fid+0
      P.putInt32be $ 3*fid+1
      P.putInt32be $ 3*fid+2
    writeVertex v = do
      P.putFloatbe $ realToFrac $ getx v
      P.putFloatbe $ realToFrac $ gety v
      P.putFloatbe $ realToFrac $ getz v

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

xyzSink :: (X a, Y a, Z a) => Environment -> ConduitT a Void IO ()
xyzSink Environment{..} = go
  where
    go = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn eHandle $ toStr v
          go
        Nothing -> pure ()
    toStr v = 
        (((show . getx $ v) ++)
      . (T.unpack eXyzVal ++)
      . ((show . gety $ v) ++)
      . (T.unpack eXyzVal ++)
      . ((show . getz $ v) ++)
      . (T.unpack eXyzLine ++))
      ""

xySink :: (X a, Y a) => Environment -> ConduitT a Void IO ()
xySink Environment{..} = go
  where
    go = do
      may <- await
      case may of
        Just v  -> do 
          liftIO $ hPutStrLn eHandle $ toStr v
          go
        Nothing -> pure ()
    toStr v = 
        (((show . getx $ v) ++)
      . (T.unpack eXyzVal ++)
      . ((show . gety $ v) ++)
      . (T.unpack eXyzLine ++))
      ""