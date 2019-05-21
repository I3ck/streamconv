{-# LANGUAGE RecordWildCards #-}

module Transformers
  ( untriple
  , triple
  ) where

import Types
import Classes
import Conduit
import System.IO
import Data.Int
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BL

--------------------------------------------------------------------------------

untriple :: (Monad m) => ConduitT (a, a, a) a m ()
untriple = go
  where 
    go = do
      may <- await
      case may of
        Nothing        -> pure ()
        Just (x, y, z) -> do
          yield x
          yield y
          yield z
          go

--------------------------------------------------------------------------------

triple :: (X a, Y a, Z a) => Environment -> ConduitT () a IO () -> ConduitT () Face IO () -> ConduitT () (Position, Position, Position) IO ()
triple Environment{..} cv cf = do
  liftIO $ runConduit $ cv .| writeVerts eTmp2
  liftIO $ runConduit $ cf .| writeFaces eTmp1
  blob <- liftIO $ BL.readFile eTmp1
  h    <- liftIO $ openFile eTmp2 ReadMode
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
writeVerts :: (X a, Y a, Z a) => String -> ConduitT a Void IO ()
writeVerts tmp = do
  h <- liftIO $ openFile tmp WriteMode
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

writeFaces :: String -> ConduitT Face Void IO ()
writeFaces tmp = do
  h <- liftIO $ openFile tmp WriteMode
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
--- TODO remove
float2beBSL :: Float -> BL.ByteString
float2beBSL = P.runPut . P.putFloatbe

--- TODO duplicate impl, should drop anyway
--- TODO remove
int2beBSL :: Int32 -> BL.ByteString
int2beBSL = P.runPut . P.putInt32be


