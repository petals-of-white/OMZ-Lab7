module Coordinates where

import           Linear
import           Monomer (Point (Point), Rect (Rect), Size (Size))

-- | Перетворення координат Monomer до координат OpenGL
monomerToGl :: Size -> Point -> (Double, Double)
monomerToGl (Size winW winH) (Point x y) = (glX, glY)
  where
    glX = -1 + 2 * x / winW
    glY = 1 - 2 * y / winH

viewportToWindow :: Size -> Rect -> (Double, Double) -> (Double, Double)
viewportToWindow winSize@(Size winW winH) _nodeContentArea@(Rect rx ry rw rh) (x,y) =
  let (transX, transY) = monomerToGl winSize (Point (rx + rw/2) (ry + rh/2)) in
  ((x - transX) * rw/winW, (y - transY) * rh/winH)


-- | 6 vertices to draw a whole 2D texture. Shoulb be used with GL_TRIANGLES
textureCoords :: [(V2 Float, V2 Float)]
textureCoords = map (\(x,z, u,v) -> (V2 x z, V2 u v))
                [ (-1, -1, 0, 0), (1, -1, 0, 1), (1, 1, 1, 1),
                  (1, 1, 1, 1), (-1, 1, 1, 0), (-1, -1, 0, 0)]

rotateXZ :: Float -> M44 Float
rotateXZ alpha =
  let t = tan (toRadians alpha) in
    V4  (V4 1 t 0 0)
        (V4 0 1 0 0)
        (V4 0 t 1 0)
        (V4 0 0 0 1)

rotToCoronal :: M44 Float
rotToCoronal = V4
  (V4 cosa  (-sina)  0 0)
   (V4 sina cosa 0 0)
   (V4 0 0 1 0)
   (V4 0 0 0 1)
  where cosa = cos (toRadians 90)
        sina = sin (toRadians 90)

scaleDown :: M44 Float
scaleDown = scaled (V4 0.5 0.5 0.5 1)


translateTo1Octet :: M44 Float
translateTo1Octet =
  V4  (V4 1 0 0 0.5)
      (V4 0 1 0 0.5)
      (V4 0 0 1 0.5)
      (V4 0 0 0   1)


toRadians :: Float -> Float
toRadians deg = deg * pi / 180

projectM :: M44 Float
-- projectM = lookAt (V3 1 1 2) (V3 0.3 0.3 0) (V3 0 0.3 0)

projectM = perspective fov aspect near far !*! lookAt (V3 1 1 2) (V3 0.3 0.3 0) (V3 0 0.3 0)
  where
    fov = toRadians 60
    aspect = 1
    near = 0.1
    far = 50

