module Sources
  ( xyz 
  , stl
  ) where

import Types
import Conduit
import Data.Text
import qualified Data.Text.Lazy as L
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