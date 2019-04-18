module Producers where

import Types
import Conduit
import qualified Parsers as P
import Text.Megaparsec

--------------------------------------------------------------------------------

xyz :: String -> String -> String -> ConduitT () Position IO ()
xyz path delimval delimline = do
    blob   <- liftIO $ readFile path
    let initial = State blob 0 (PosState blob 0 (SourcePos "" (mkPos 0) (mkPos 0)) (mkPos 0) "") ---TODO PosState constructor likely incorrect
    go initial
  where
    go input = do
      let (newinput, parsed) = runParser' (P.xyzLine delimval delimline) input
      case parsed of
        Left _  -> pure () --TODO consider error handling
        Right x -> do
          yield x
          go newinput