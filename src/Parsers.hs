{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Types
import Data.Void
import Data.Text.Lazy
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

--- TODO not parsing lazyly
xyz :: Text -> Text -> Parser [Position]
xyz delimval delimline = many $ xyzLine delimval delimline ---TODO use delimline only in this parser and dont require it at the end

xyzLine :: Text -> Text -> Parser Position
xyzLine delimval delimline = do
  x <- L.float
  string delimval
  y <- L.float
  string delimval
  z <- L.float
  string delimline

  pure $ Position x y z

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--double :: Parser Double
--double = try (L.signed sc L.float) <|> (fromIntegral <$> L.signed sc L.decimal)

--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
