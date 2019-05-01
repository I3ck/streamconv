{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( xyzLine
  , skipSTLAsciiHeader
  , stlFace
  , plyVertex
  , plyFace
  ) where

import Types
import Data.Text
import Data.Attoparsec.Text.Lazy

--------------------------------------------------------------------------------

xyzLine :: Text -> Text -> Parser Position
xyzLine delimval delimline = do
  x <- double
  string delimval
  y <- double
  string delimval
  z <- double
  string delimline

  pure $ Position x y z

--------------------------------------------------------------------------------

skipSTLAsciiHeader :: Parser ()
skipSTLAsciiHeader = do
  string "solid"
  skipRestOfLine

--------------------------------------------------------------------------------
  
--- TODO later also yield Normals
--- TODO likely misses space skipping
stlFace :: Parser (Position, Position, Position)
stlFace = do
  skipSpace
  string "facet normal"
  skipRestOfLine --TODO later parse normals here
  skipSpace
  string "outer loop"
  skipRestOfLine
  a <- vertex
  b <- vertex
  c <- vertex
  skipSpace
  string "endloop"
  skipRestOfLine
  skipSpace
  string "endfacet"
  skipRestOfLine
  pure $ (a, b, c)

  where
    vertex :: Parser Position
    vertex = do
      skipSpace
      string "vertex"
      skipSpace
      x <- double
      skipSpace
      y <- double
      skipSpace
      z <- double
      skipRestOfLine
      pure $ Position x y z

--------------------------------------------------------------------------------

--- TODO currently assumes x y z and skipping rest of line
--- TODO won't do in final version, must be able to order freely
plyVertex :: Parser Position
plyVertex = do
  x <- double
  skipSpace
  y <- double
  skipSpace
  z <- double
  skipRestOfLine --- TODO assumes x y z then nothing
  pure $ Position x y z

--------------------------------------------------------------------------------

--- TODO assumes 3 index faces
plyFace :: Parser Face
plyFace = do
  string "3"
  skipSpace
  a <- decimal
  skipSpace
  b <- decimal
  skipSpace
  c <- decimal
  skipRestOfLine
  pure $ Face a b c

--------------------------------------------------------------------------------

skipRestOfLine :: Parser ()
skipRestOfLine = do
  takeTill isEndOfLine
  endOfLine