module Producers where

import Types
import Conduit
import Data.Text
import qualified Data.Text.Lazy.IO as L
import qualified Parsers as P
import Data.Attoparsec.Text.Lazy as A

--------------------------------------------------------------------------------

xyz :: String -> Text -> Text -> ConduitT () Position IO ()
xyz path delimval delimline = do
    blob   <- liftIO $ L.readFile path
    go blob
  where
    go input = do
      let result = A.parse (P.xyzLine delimval delimline) input
      case result of
        A.Fail _ _ _ -> pure ()
        A.Done rest x -> do
          yield x
          go rest