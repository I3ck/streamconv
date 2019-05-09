module Types where

--------------------------------------------------------------------------------

data Position = Position
  { x :: Double
  , y :: Double
  , z :: Double
  } deriving (Show)

data Normal = Normal
  { i :: Double
  , j :: Double
  , k :: Double
  } deriving (Show)

data Face = Face
  { a :: Int
  , b :: Int
  , c :: Int
  } deriving (Show)

data Format
  = StlAscii
  | StlBinary
  | Obj
  | PlyAscii
  | PlyBinary
  | Xyz