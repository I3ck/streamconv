{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Types
import Data.Void
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Attoparsec.Text.Lazy as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LE


--------------------------------------------------------------------------------

type Parser = Parsec Void L.Text

--------------------------------------------------------------------------------

--- TODO not parsing lazyly
xyz :: L.Text -> L.Text -> Parser [Position]
xyz delimval delimline = many $ xyzLine delimval delimline ---TODO use delimline only in this parser and dont require it at the end

xyzLine :: L.Text -> L.Text -> Parser Position
xyzLine delimval delimline = do
  x <- LE.float
  string delimval
  y <- LE.float
  string delimval
  z <- LE.float
  string delimline

  pure $ Position x y z

xyzLine' :: Text -> Text -> A.Parser Position
xyzLine' delimval delimline = do
  x <- A.double
  A.string delimval
  y <- A.double
  A.string delimval
  z <- A.double
  A.string delimline

  pure $ Position x y z


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--double :: Parser Double
--double = try (L.signed sc L.float) <|> (fromIntegral <$> L.signed sc L.decimal)

--------------------------------------------------------------------------------

sc :: Parser ()
sc = LE.space space1 lineComment blockComment
  where
    lineComment  = LE.skipLineComment "//"
    blockComment = LE.skipBlockComment "/*" "*/"
