module Types where

data SimpleImage p = SimpleImage {imgPixels :: [p], imgRows :: Int, imgColumns :: Int}
    deriving (Eq, Show)


data Plane = Saggital | Coronal | Transverse deriving (Eq, Show)

data AppModel p =
  AppModel { 
    appImages :: [SimpleImage p],
    appRotationXZ :: Int,
    appPlane :: Plane,
    appY :: Float 
  }
  deriving (Eq, Show)

data AppEvent = ChangeRotDeg Int | SetPlane Plane | MoveY Float | MoveCamera Float
  deriving (Eq, Show)
