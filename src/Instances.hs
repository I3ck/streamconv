{-# LANGUAGE FlexibleInstances #-}

module Instances where

import           Classes
import           Types

--------------------------------------------------------------------------------

instance X Position where
  getx = x

instance Y Position where
  gety = y

instance Z Position where
  getz = z

instance OptX Position
instance OptY Position
instance OptZ Position

--------------------------------------------------------------------------------

instance I Normal where
  geti = i

instance J Normal where
  getj = j

instance K Normal where
  getk = k

instance OptI Normal
instance OptJ Normal
instance OptK Normal
