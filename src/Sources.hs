module Sources
  ( xyz 
  , stl
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

--------------------------------------------------------------------------------

xyz :: (Monad m) => L.Text -> Text -> Text -> ConduitT () Position m ()
xyz blob delimval delimline = go blob
  where
    go input = case A.parse (P.xyzLine delimval delimline) input of
        A.Fail{}      -> pure ()
        A.Done rest x -> do
          yield x
          go rest

--------------------------------------------------------------------------------

stl :: (Monad m) => L.Text -> ConduitT () (Position, Position, Position) m ()
stl blob = case A.parse P.skipSTLAsciiHeader blob of
    A.Fail{}      -> pure ()
    A.Done rest _ -> go rest
  where
    go input = case A.parse P.stlFace input of
        A.Fail{}      -> pure ()
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
plyVertices blob = case A.parse P.plyHeader blob of
    A.Fail{}      ->  pure ()
    A.Done rest _ -> go rest
  where
    go input = case A.parse P.plyVertex input of
        A.Fail{}      -> pure ()
        A.Done rest x -> do
          yield x
          go rest

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
objVertices = go
  where
    go input = case A.parse P.objVertex input of
        A.Fail{}      -> pure ()
        A.Done rest x -> do
          yield x
          go rest

--------------------------------------------------------------------------------

--- TODO wont work if faces and vertices not in expected order, required in format?
objFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
objFaces blob = case A.parse (many' P.objVertex) blob of
    A.Fail{}      -> pure ()
    A.Done rest _ -> go rest
  where
    go input = case A.parse P.objFace input of
        A.Fail{}      -> pure ()
        A.Done rest x -> do
          yield x
          go rest

--------------------------------------------------------------------------------

-- skip header and vertices then parse faces
-- TODO must skip comments
plyFaces :: (Monad m) => L.Text -> ConduitT () Face m ()
plyFaces blob = case A.parse P.plyHeader blob of
    A.Fail{}      ->  pure ()
    A.Done rest _ -> case A.parse (many' P.plyVertex) rest of
        A.Fail{}      -> pure ()
        A.Done rest _ -> go rest
  where
    go input = case A.parse P.plyFace input of
        A.Fail{}      -> pure ()
        A.Done rest x -> do
          yield x
          go rest
