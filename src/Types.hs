module Types where

import qualified Data.ByteString.Lazy as BL
import           Data.Text
import qualified Data.Text.Lazy       as L
import           System.IO

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

--------------------------------------------------------------------------------

data Environment = Environment
  { eBlobA1  :: L.Text        -- File read as lazy text
  , eBlobA2  :: L.Text        -- File read as lazy text
  , eBlobB1  :: BL.ByteString -- File read as lazy bytestring
  , eBlobB2  :: BL.ByteString -- File read as lazy bytestring
  , eXyzVal  :: Text          -- Value delimiter for xyz files
  , eXyzLine :: Text          -- Line delimiter for xyz files
  , eHandle  :: Handle        -- Handle to write to
  , eTmp1    :: String        -- Path to first tmp file
  , eTmp2    :: String        -- Path to second tmp file
  }

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data ArgsRaw = ArgsRaw
  { rpIn     :: String
  , rpOut    :: String
  , rfIn     :: String
  , rfOut    :: String
  , rTmp1    :: String
  , rTmp2    :: String
  , rXyzVal  :: String
  , rXyzLine :: String
  , rList    :: Bool
  }

data Args = Args
  { pIn     :: String
  , pOut    :: String
  , fIn     :: Format
  , fOut    :: Format
  , tmp1    :: String
  , tmp2    :: String
  , xyzVal  :: String
  , xyzLine :: String
  , list    :: Bool
  }
