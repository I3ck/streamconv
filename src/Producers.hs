module Producers where

import Types
import Conduit
import qualified Parsers as P
import Text.Megaparsec

--------------------------------------------------------------------------------

xyz :: String -> String -> String -> ConduitT () Position IO ()
xyz path delimval delimline = do
  blob   <- liftIO $ readFile path
  let parsed = parse (P.xyz delimval delimline) "" blob
  case parsed of
    Left _  -> pure () --TODO consider error handling
    Right x -> yieldMany x