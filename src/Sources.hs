module Sources
  ( xyz 
  , stlAscii
  , stlBinary
  , ply
  , obj
  ) where

import Types
import Conduit
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import qualified Parsers as P
import Data.Attoparsec.Text.Lazy as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL


--------------------------------------------------------------------------------

xyz :: (Monad m) => L.Text -> Text -> Text -> ConduitT () Position m ()
xyz blob delimval delimline = makeSource (pure ()) (P.xyzLine delimval delimline) blob

--------------------------------------------------------------------------------

stlAscii :: (Monad m) => L.Text -> ConduitT () (Position, Position, Position) m ()
stlAscii = makeSource P.skipSTLAsciiHeader P.stlFace

--------------------------------------------------------------------------------

stlBinary :: (Monad m) => BL.ByteString -> ConduitT () (Position, Position, Position) m ()
stlBinary blob = go $ (BL.drop $ 10 + 4) blob -- 80 bits for header, 32 bit for triangle cound
  where
    go input = case G.runGetIncremental getVertex `G.pushChunks` BL.take 50 input of
        G.Fail{}    -> pure ()
        G.Partial{} -> pure ()
        G.Done _ _ x -> do
          yield x
          go $ BL.drop 50 input
    getVertex = do
      G.getFloatle             -- 4 3 normals, ignored for now
      G.getFloatle             -- 8
      G.getFloatle             -- 12
      ax <- c <$> G.getFloatle -- 16
      ay <- c <$> G.getFloatle -- 20
      az <- c <$> G.getFloatle -- 24
      bx <- c <$> G.getFloatle -- 28
      by <- c <$> G.getFloatle -- 32
      bz <- c <$> G.getFloatle -- 36
      cx <- c <$> G.getFloatle -- 40
      cy <- c <$> G.getFloatle -- 44
      cz <- c <$> G.getFloatle -- 48
      G.getInt16le             -- 50
      pure (Position ax ay az, Position bx by bz, Position cx cy cz)
    c = realToFrac

--------------------------------------------------------------------------------

ply :: (Monad m) => String -> IO (ConduitT () Position m (), ConduitT () Face m ())
ply path = do
  -- important to read file twice to ensure no space leak
  bloba <- liftIO $ LIO.readFile path
  blobb <- liftIO $ LIO.readFile path
  pure (plyVertices bloba, plyFaces blobb)


--------------------------------------------------------------------------------
---TODO must skip comments
plyVertices :: (Monad m) => L.Text -> ConduitT () Position m ()
plyVertices = makeSource P.plyHeader P.plyVertex

--------------------------------------------------------------------------------

obj:: (Monad m) => String -> IO (ConduitT () Position m (), ConduitT () Face m ())
obj path = do
  -- important to read file twice to ensure no space leak
  bloba <- liftIO $ LIO.readFile path
  blobb <- liftIO $ LIO.readFile path
  pure (objVertices bloba, objFaces blobb)

--------------------------------------------------------------------------------
--- TODO wont work if faces and vertices not in expected order, required in format?
objVertices :: (Monad m) => L.Text -> ConduitT () Position m ()
objVertices = makeSource (pure ()) P.objVertex

--------------------------------------------------------------------------------

--- TODO wont work if faces and vertices not in expected order, required in format?
objFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
objFaces = makeSource (many' P.objVertex >> pure ()) P.objFace

--------------------------------------------------------------------------------

-- skip header and vertices then parse faces
-- TODO must skip comments
plyFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
plyFaces = makeSource (P.plyHeader >> many' P.plyVertex >> pure() ) P.plyFace

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
