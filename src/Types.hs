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
  deriving (Show, Read)

formats = 
  [ StlAscii
  , StlBinary
  , Obj
  , PlyAscii
  , PlyBinary
  , Xyz
  ]

data ArgsRaw = ArgsRaw
  { rpIn  :: String
  , rpOut :: String
  , rfIn  :: String
  , rfOut :: String
  }

data Args = Args
  { pIn  :: String
  , pOut :: String
  , fIn  :: Format
  , fOut :: Format
  }