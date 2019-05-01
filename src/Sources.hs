module Sources
  ( xyz 
  , stl
  , ply
  ) where

import Types
import Conduit
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import qualified Parsers as P
import Data.Attoparsec.Text.Lazy as A

--------------------------------------------------------------------------------

xyz :: (Monad m) => L.Text -> Text -> Text -> ConduitT () Position m ()
xyz blob delimval delimline = go blob
  where
    go input = do
      let result = A.parse (P.xyzLine delimval delimline) input
      case result of
        A.Fail _ _ _ -> pure ()
        A.Done rest x -> do
          yield x
          go rest

--------------------------------------------------------------------------------

stl :: (Monad m) => L.Text -> ConduitT () (Position, Position, Position) m ()
stl blob = do
  let result = A.parse P.skipSTLAsciiHeader blob
  case result of
    A.Fail _ _ _  -> pure ()
    A.Done rest _ -> go rest
  where
    go input = do
      let result = A.parse P.stlFace input
      case result of
        A.Fail _ _ _ -> pure ()
        A.Done rest x -> do
          yield x
          go rest

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
plyVertices blob = do
  let result = A.parse P.plyHeader blob
  case result of
    A.Fail _ _ _ ->  pure ()
    A.Done rest _ -> go rest
  where
    go input = do
      let result = A.parse P.plyVertex input
      case result of
        A.Fail _ _ _ -> pure ()
        A.Done rest x -> do
          yield x
          go rest

--------------------------------------------------------------------------------

-- skip header and vertices then parse faces
-- TODO must skip comments
plyFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
plyFaces blob = do
  let result = A.parse P.plyHeader blob
  case result of
    A.Fail _ _ _ ->  pure ()
    A.Done rest _ -> do
      let result = A.parse (many' P.plyVertex) rest
      case result of
        A.Fail _ _ _  -> pure ()
        A.Done rest _ -> go rest
  where
    go input = do
      let result = A.parse P.plyFace input
      case result of
        A.Fail _ _ _ -> pure ()
        A.Done rest x -> do
          yield x
          go rest
