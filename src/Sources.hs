{-# LANGUAGE RecordWildCards #-}

module Sources
  ( xyz 
  , stlAscii
  , stlBinary
  , ply
  , obj
  , off
  , triplet

  , posSources
  , pfSources
  , tripletSources
  ) where

import Types
import Conduit
import Classes
import System.IO
import Data.Int
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Parsers as P
import Data.Attoparsec.Text.Lazy as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P

posSources :: M.Map Format (Environment -> ConduitT () Position IO ())
posSources = M.fromList 
  [ (Xyz, xyz)
  ]

tripletSources :: M.Map Format (Environment -> ConduitT () (Position, Position, Position) IO ())
tripletSources = M.fromList
  [ (StlAscii,  stlAscii)
  , (StlBinary, stlBinary)
  ]

pfSources :: M.Map Format (Environment -> (ConduitT () Position IO (), ConduitT () Face IO ()))
pfSources = M.fromList
  [ (Obj,      obj)
  , (PlyAscii, ply)
  , (Off,      off)
  ]

--------------------------------------------------------------------------------

xyz :: (Monad m) => Environment -> ConduitT () Position m ()
xyz Environment{..} = makeSource (pure ()) (P.xyzLine eXyzVal eXyzLine) eBlobA1

--------------------------------------------------------------------------------

stlAscii :: (Monad m) => Environment -> ConduitT () (Position, Position, Position) m ()
stlAscii Environment{..} = makeSource P.skipSTLAsciiHeader P.stlFace eBlobA1

--------------------------------------------------------------------------------

stlBinary :: (Monad m) => Environment -> ConduitT () (Position, Position, Position) m ()
stlBinary Environment{..} = go $ (BL.drop $ 80 + 4) eBlobB1 -- 80 bytes for header, 32 bit for triangle count
  where
    go input = case G.runGetIncremental getVertex `G.pushChunks` BL.take 50 input of
        G.Fail{}    -> pure ()
        G.Partial{} -> pure ()
        G.Done _ _ x -> do
          yield x
          go $ BL.drop 50 input
    getVertex = do
      G.getFloatle         -- 4 3 normals, ignored for now
      G.getFloatle         -- 8
      G.getFloatle         -- 12
      ax <- c G.getFloatle -- 16
      ay <- c G.getFloatle -- 20
      az <- c G.getFloatle -- 24
      bx <- c G.getFloatle -- 28
      by <- c G.getFloatle -- 32
      bz <- c G.getFloatle -- 36
      cx <- c G.getFloatle -- 40
      cy <- c G.getFloatle -- 44
      cz <- c G.getFloatle -- 48
      G.getInt16le             -- 50
      pure (Position ax ay az, Position bx by bz, Position cx cy cz)
    c = fmap realToFrac

--------------------------------------------------------------------------------

--- TODO constants file or settings or parameter?
tmpVertices :: String
tmpVertices = "streamconvVerts.tmp"

tmpFaces :: String
tmpFaces = "streamconvFaces.tmp"

--- TODO move to Transformers?
--- TODO better name
triplet :: (X a, Y a, Z a) => ConduitT () a IO () -> ConduitT () Face IO () -> ConduitT () (Position, Position, Position) IO ()
triplet cv cf = do
  liftIO $ runConduit $ cv .| writeVerts
  liftIO $ runConduit $ cf .| writeFaces
  blob <- liftIO $ BL.readFile tmpFaces
  h    <- liftIO $ openFile tmpVertices ReadMode
  go h blob
  --- TODO read face blob and index access into positions
  --- TODO consider using seek + handle
  liftIO $ hClose h
  where
    go h input = case G.runGetIncremental getFace `G.pushChunks` BL.take 12 input of
      G.Fail{}                -> pure ()
      G.Partial{}             -> pure ()
      G.Done _ _ (Face a b c) -> do
        mva <- liftIO $ fetchVertex h $ fromIntegral a
        mvb <- liftIO $ fetchVertex h $ fromIntegral b
        mvc <- liftIO $ fetchVertex h $ fromIntegral c
        case (mva, mvb, mvc) of
          (Just va, Just vb, Just vc) -> do
            yield (va, vb, vc)
            go h (BL.drop 12 input)
          _ -> pure ()

        --- TODO seek to a b c in h and if all suceed, yield triplet


    getFace = do
      a <- G.getInt32be -- 4
      b <- G.getInt32be -- 8
      c <- G.getInt32be -- 12
      pure $ Face (fromIntegral a) (fromIntegral b) (fromIntegral c)

    fetchVertex h i = do
      hSeek h AbsoluteSeek (i * 12)
      blobv <- BL.hGet h 12
      case G.runGetIncremental getVertex `G.pushChunks` blobv of
        G.Fail{}     -> pure Nothing
        G.Partial{}  -> pure Nothing
        G.Done _ _ v -> pure . pure $ v


    getVertex = do
      x <- G.getFloatbe -- 4
      y <- G.getFloatbe -- 8
      z <- G.getFloatbe -- 12
      pure $ Position (realToFrac x) (realToFrac y) (realToFrac z)




--- TODO consider moving these helpers?
--- TODO consider delete of tmp file
writeVerts :: (X a, Y a, Z a) => ConduitT a Void IO ()
writeVerts = do
  h <- liftIO $ openFile tmpVertices WriteMode
  go h
  liftIO $ hClose h
  where
    go h = do
      may <- await
      case may of
        Nothing -> pure ()
        Just v  -> do 
          liftIO $ do
            ---TODO conversion to float currently, keep double precision!
            BL.hPutStr h $ float2beBSL $ realToFrac $ getx $ v
            BL.hPutStr h $ float2beBSL $ realToFrac $ gety $ v
            BL.hPutStr h $ float2beBSL $ realToFrac $ getz $ v
          go h

writeFaces :: ConduitT Face Void IO ()
writeFaces = do
  h <- liftIO $ openFile tmpFaces WriteMode
  go h
  liftIO $ hClose h
  where
    go h = do
      may <- await
      case may of
        Nothing -> pure ()
        Just (Face a b c) -> do 
          liftIO $ do
            --- TODO MIGHT be lossy
            BL.hPutStr h $ int2beBSL $ fromIntegral a
            BL.hPutStr h $ int2beBSL $ fromIntegral b
            BL.hPutStr h $ int2beBSL $ fromIntegral c
          go h

--- TODO duplicate impl, should drop anyway
float2beBSL :: Float -> BL.ByteString
float2beBSL = P.runPut . P.putFloatbe

--- TODO duplicate impl, should drop anyway
int2beBSL :: Int32 -> BL.ByteString
int2beBSL = P.runPut . P.putInt32be

--------------------------------------------------------------------------------

ply :: (Monad m) => Environment -> (ConduitT () Position m (), ConduitT () Face m ())
ply Environment{..} = (plyVertices eBlobA1, plyFaces eBlobA2)


--------------------------------------------------------------------------------
---TODO must skip comments
plyVertices :: (Monad m) => L.Text -> ConduitT () Position m ()
plyVertices = makeSource P.plyHeader P.plyVertex

--------------------------------------------------------------------------------

-- skip header and vertices then parse faces
-- TODO must skip comments
plyFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
plyFaces = makeSource (P.plyHeader >> many' P.plyVertex >> pure() ) P.plyFace

--------------------------------------------------------------------------------

off :: (Monad m) => Environment -> (ConduitT () Position m (), ConduitT () Face m ())
off Environment{..} = (offVertices eBlobA1, offFaces eBlobA2)

--------------------------------------------------------------------------------
---TODO must skip comments
offVertices :: (Monad m) => L.Text -> ConduitT () Position m ()
offVertices = makeSource P.skipOffHeader P.offVertex

--------------------------------------------------------------------------------

-- skip header and vertices then parse faces
-- TODO must skip comments
offFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
offFaces = makeSource (P.skipOffHeader >> many' P.offVertex >> pure() ) P.offFace

--------------------------------------------------------------------------------

obj:: (Monad m) => Environment -> (ConduitT () Position m (), ConduitT () Face m ())
obj Environment{..} = (objVertices eBlobA1, objFaces eBlobA2)

--------------------------------------------------------------------------------
--- TODO wont work if faces and vertices not in expected order, required in format?
objVertices :: (Monad m) => L.Text -> ConduitT () Position m ()
objVertices = makeSource (pure ()) P.objVertex

--------------------------------------------------------------------------------

--- TODO wont work if faces and vertices not in expected order, required in format?
objFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
objFaces = makeSource (many' P.objVertex >> pure ()) P.objFace

--------------------------------------------------------------------------------
--- TODO monad restriction not required?
makeSource :: (Monad m) => A.Parser () -> A.Parser a -> L.Text -> ConduitT () a m ()
makeSource skipper parser blob = case A.parse skipper blob of
    A.Fail{}      -> pure ()
    A.Done rest _ -> go rest
  where
    go input = case A.parse parser input of
      A.Fail{}      -> pure ()
      A.Done rest x -> do
        yield x
        go rest
