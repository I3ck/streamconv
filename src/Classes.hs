{-# LANGUAGE DefaultSignatures #-}

module Classes where

--------------------------------------------------------------------------------

class OptX a where
  getoptx :: a -> Maybe Double
  default getoptx :: X a => a -> Maybe Double
  getoptx = pure . getx

class OptY a where
  getopty :: a -> Maybe Double
  default getopty :: Y a => a -> Maybe Double
  getopty = pure . gety

class OptZ a where
  getoptz :: a -> Maybe Double
  default getoptz :: Z a => a -> Maybe Double
  getoptz = pure . getz

--------------------------------------------------------------------------------

class (OptX a) => X a where
  getx :: a -> Double

class (OptY a) => Y a where
  gety :: a -> Double

class (OptZ a) => Z a where
  getz :: a -> Double

--------------------------------------------------------------------------------

class OptI a where
  getopti :: a -> Maybe Double
  default getopti :: I a => a -> Maybe Double
  getopti = pure . geti

class OptJ a where
  getoptj :: a -> Maybe Double
  default getoptj :: J a => a -> Maybe Double
  getoptj = pure . getj

class OptK a where
  getoptk :: a -> Maybe Double
  default getoptk :: K a => a -> Maybe Double
  getoptk = pure . getk

--------------------------------------------------------------------------------

class (OptI a) => I a where
  geti :: a -> Double

class (OptJ a) => J a where
  getj :: a -> Double

class (OptK a) => K a where
  getk :: a -> Double