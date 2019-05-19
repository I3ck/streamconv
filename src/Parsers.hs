{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( Parsers.xyzLine
  , skipSTLAsciiHeader
  , stlFace
  , plyVertex
  , plyFace
  , plyComment
  , plyHeader --TODO inconsistent names, some prefixed with "skip" others not
  , offVertex
  , offFace
  , offComment
  , skipOffHeader
  , objComment
  , objVertex
  , objFace
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

skipOffHeader :: Parser ()
skipOffHeader = do
  string "OFF"
  skipAllRestOfLine
  skipAllRestOfLine

--------------------------------------------------------------------------------

offVertex :: Parser Position
offVertex = plyVertex

--------------------------------------------------------------------------------

offFace :: Parser Face
offFace = plyFace

--------------------------------------------------------------------------------

offComment :: Parser ()
offComment = do
  string "#"
  skipAllRestOfLine

--------------------------------------------------------------------------------

skipSTLAsciiHeader :: Parser ()
skipSTLAsciiHeader = do
  string "solid"
  skipAllRestOfLine

--------------------------------------------------------------------------------
  
--- TODO later also yield Normals
--- TODO likely misses space skipping
stlFace :: Parser (Position, Position, Position)
stlFace = do
  skipSpace
  string "facet normal"
  skipAllRestOfLine --TODO later parse normals here
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
  pure (a, b, c)

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
  --todo enforce exactly 3 values here to not match on face definitions for now
  skipRestOfLine--- TODO assumes x y z then nothing
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

plyComment :: Parser ()
plyComment = do
  string "comment"
  skipAllRestOfLine

--------------------------------------------------------------------------------
---TODO should fail if format does not match what's expected
---TODO add skip to name?
plyHeader :: Parser ()
plyHeader = do
  manyTill anyChar (string "end_header")
  skipRestOfLine

--------------------------------------------------------------------------------

objComment :: Parser ()
objComment = do
  string "#"
  skipAllRestOfLine

--------------------------------------------------------------------------------

objVertex :: Parser Position
objVertex = do
  skipObjComments
  string "v"
  skipSpace
  x <- double
  skipSpace
  y <- double
  skipSpace
  z <- double
  skipAllRestOfLine --- TODO consider reading W component
  pure $ Position x y z

--------------------------------------------------------------------------------

--- TODO assumes only the vertex syntax
--- would fail if normal or texture index is passed
--- TODO doesnt allow negative indices of obj / doesnt convert correctly
objFace :: Parser Face
objFace = do
  skipObjComments
  string "f"
  skipSpace
  a <- decimal
  skipSpace
  b <- decimal
  skipSpace
  c <- decimal
  skipRestOfLine
  pure $ Face (a-1) (b-1) (c-1) -- obj indexed starting at 1

--------------------------------------------------------------------------------

skipObjComments :: Parser ()
skipObjComments = do 
  many' (skipSpace >> objComment >> skipSpace)
  pure ()

--------------------------------------------------------------------------------

skipRestOfLine :: Parser () --TODO consider rename (skips any space till end of line and that end of line)
skipRestOfLine = do
  skipInlineSpace
  endOfLine

--------------------------------------------------------------------------------

skipAllRestOfLine :: Parser () --TODO consider rename (skips everything and end of line)
skipAllRestOfLine = do
  takeTill isEndOfLine
  endOfLine

--------------------------------------------------------------------------------

skipInlineSpace :: Parser ()
skipInlineSpace = do 
  many' $ skip (\x -> x == ' ' || x == '\t')
  pure ()