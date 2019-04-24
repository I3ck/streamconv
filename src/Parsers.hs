{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( xyzLine
  , skipSTLAsciiHeader
  , stlFace
  ) where

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

--------------------------------------------------------------------------------

skipSTLAsciiHeader :: A.Parser ()
skipSTLAsciiHeader = do
  A.string "solid"
  skipRestOfLine

--------------------------------------------------------------------------------
  
--- TODO later also yield Normals
--- TODO likely misses space skipping
stlFace :: A.Parser (Position, Position, Position)
stlFace = do
  A.string "facet"
  skipRestOfLine --TODO later parse normals here
  A.string "outer loop"
  skipRestOfLine
  a <- vertex
  b <- vertex
  c <- vertex
  A.string "endloop"
  skipRestOfLine
  A.string "endfacet"
  skipRestOfLine
  pure $ (a, b, c)

  where
    vertex :: A.Parser Position
    vertex = do
      A.string "vertex"
      A.skipSpace
      x <- A.double
      A.skipSpace
      y <- A.double
      A.skipSpace
      z <- A.double
      skipRestOfLine
      pure $ Position x y z




--------------------------------------------------------------------------------

skipRestOfLine :: A.Parser ()
skipRestOfLine = do
  A.takeTill A.isEndOfLine
  A.endOfLine