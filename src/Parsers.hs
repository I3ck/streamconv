module Parsers where

import Types
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------

type Parser = Parsec Void String

--------------------------------------------------------------------------------

--- TODO not parsing lazyly
xyz :: String -> String -> Parser [Position]
xyz delimval delimline = many $ xyzLine delimval delimline ---TODO use delimline only in this parser and dont require it at the end

xyzLine :: String -> String -> Parser Position
xyzLine delimval delimline = do
  x <- double
  string delimval
  y <- double
  string delimval
  z <- double
  string delimline

  pure $ Position x y z

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

double :: Parser Double
double = try (L.signed sc L.float) <|> (fromIntegral <$> L.signed sc L.decimal)

--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
