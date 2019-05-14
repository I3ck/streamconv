module Types where

import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as BL

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

data SourceData = SourceData
  { sBlobA1  :: L.Text        -- File read as lazy text
  , sBlobA2  :: L.Text        -- File read as lazy text
  , sBlobB1  :: BL.ByteString -- File read as lazy bytestring
  , sBlobB2  :: BL.ByteString -- File read as lazy bytestring
  , sXyzVal  :: Text          -- value delimiter for xyz files
  , sXyzLine :: Text          -- line delimiter for xyz files
  }

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