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
xyz blob delimval delimline = makeSource (pure ()) (P.xyzLine delimval delimline) blob

--------------------------------------------------------------------------------

stl :: (Monad m) => L.Text -> ConduitT () (Position, Position, Position) m ()
stl = makeSource P.skipSTLAsciiHeader P.stlFace

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
