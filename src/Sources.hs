{-# LANGUAGE RecordWildCards #-}

module Sources
  ( xyz 
  , stlAscii
  , stlBinary
  , ply
  , obj
  , off
  , posSources
  , pfSources
  , tripletSources
  ) where

import Types
import Conduit
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Parsers as P
import Data.Attoparsec.Text.Lazy as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL

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
