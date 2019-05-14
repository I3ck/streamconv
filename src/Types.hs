module Types where

import Data.Text
import System.IO
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

data Environment = Environment
  { eBlobA1  :: L.Text        -- file read as lazy text
  , eBlobA2  :: L.Text        -- file read as lazy text
  , eBlobB1  :: BL.ByteString -- file read as lazy bytestring
  , eBlobB2  :: BL.ByteString -- file read as lazy bytestring
  , eXyzVal  :: Text          -- value delimiter for xyz files
  , eXyzLine :: Text          -- line delimiter for xyz files
  , eHandle  :: Handle        -- handle to write to
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