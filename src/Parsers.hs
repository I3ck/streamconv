{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Types
import Data.Text
import qualified Data.Attoparsec.Text.Lazy as A

--------------------------------------------------------------------------------

xyzLine :: Text -> Text -> A.Parser Position
xyzLine delimval delimline = do
  x <- A.double
  A.string delimval
  y <- A.double
  A.string delimval
  z <- A.double
  A.string delimline

  pure $ Position x y z