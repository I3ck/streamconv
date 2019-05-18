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
  , eTmp1    :: String        -- path to first tmp file
  , eTmp2    :: String        -- path to second tmp file
  }

data Format -- MAKE SURE TO UPDATE formats BELOW!!!
  = StlAscii
  | StlBinary
  | Obj
  | Off
  | PlyAscii
  | PlyBinary
  | Xy
  | Xyz
  deriving (Show, Read, Eq, Ord)

formats = 
  [ StlAscii
  , StlBinary
  , Obj
  , Off
  , PlyAscii
  , PlyBinary
  , Xy
  , Xyz
  ]

data ArgsRaw = ArgsRaw
  { rpIn  :: String
  , rpOut :: String
  , rfIn  :: String
  , rfOut :: String
  , rTmp1 :: String
  , rTmp2 :: String
  , rList :: Bool
  }

data Args = Args
  { pIn  :: String
  , pOut :: String
  , fIn  :: Format
  , fOut :: Format
  , tmp1 :: String
  , tmp2 :: String
  , list :: Bool
  }