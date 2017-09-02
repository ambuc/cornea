module Cornea
    ( Obj (..) 
    , World 
    , isometric 
    , mark 
    , render 
    , seenFrom 
    , solid
    ) where

import Codec.Picture                       ( PixelRGBA8 (..), writePng, Image )
import Data.Monoid                         ( (<>) )
import Data.Function                       ( on )
import Data.List                           ( transpose, genericLength, sortBy )
import Data.Ord                            ( comparing )
import Data.Matrix                         ( Matrix, fromList, toList, transpose )
import Graphics.Rasterific                 ( Drawing, Cap (..), Geometry,
                                             Join (..), Primitive, circle, fill,
                                             polygon, polyline, renderDrawing,
                                             stroke, withTexture,
                                             withTransformation )
import Graphics.Rasterific.Linear          ( V2 (..) )
import Graphics.Rasterific.Texture         ( uniformTexture )
import Graphics.Rasterific.Transformations (translate, scale )

data Obj   = Cord [Float] | Edge [[Float]] | Face [[Float]] deriving (Eq, Show)
type Style = ([Primitive] -> Drawing PixelRGBA8 ())
type World = [(Obj, Style)]
type View  = (Float,Float)

-- more-or-less synonyms for common styling patterns. useful in constructing
-- more terse Worlds.
solid :: Geometry geom => PixelRGBA8 -> geom -> Drawing PixelRGBA8 ()
solid k = withTexture (uniformTexture k) . fill

mark :: Geometry geom => PixelRGBA8 -> Float -> geom -> Drawing PixelRGBA8 ()
mark k n = withTexture (uniformTexture k)
         . stroke n JoinRound (CapRound, CapRound)

--isometric is the default viewpoint. you can also construct your own with a
--(pitch, yaw) angle, where pitch is anywhere between -90 and +90, and
--                            yaw is anywhere between   0 and 360.
isometric :: (Float,Float)
isometric = (35.264, 45)

-- https://en.wikipedia.org/wiki/Isometric_projection#Mathematics
metric :: (Float, Float) -> Matrix Float
metric (p, w) = m1 * m2
  where m1 = fromList 3 3 [1, 0, 0, 0, 1, 0, 0, 0, 0]
        m2 = fromList 3 3 [1, 0, 0, 0, cos a, sin a, 0, -sin a, -cos a]
           * fromList 3 3 [cos b, 0, -sin b, 0, 1, 0, sin b, 0, cos b]
          where a = p * pi / 180; b = w * pi * 2 / 360

-- We must sort by camera proximity before rendering.
-- Because the camera in an isometric projection has a pitch and yaw, but no set
-- distance from the origin, we must sort by the scalar product of the
-- origin-centroid vector with the line of sight.
--
-- https://en.wikipedia.org/wiki/Visibility_(geometry)
seenFrom :: World -> (Float,Float) -> Drawing PixelRGBA8 ()
world `seenFrom` v = mapM_ (`drawFrom` v)
                   $ sortBy (comparing $ closeness v . centroid . fst) world

-- drawFrom is an infix, so that we can write <world> `drawFrom` <viewpoint>.
drawFrom :: (Obj, [Primitive] -> t) -> (Float, Float) -> t
(Cord coord, sty) `drawFrom` v = sty $ circle         (proj v $ Cord coord) 1
(Edge pts  , sty) `drawFrom` v = sty $ polyline $ map (proj v . Cord) pts
(Face pts  , sty) `drawFrom` v = sty $ polygon  $ map (proj v . Cord) pts

-- proj only projects a 3d point from a viewpoint. The other objects get mapped
-- in drawFrom
proj :: (Float,Float) -> Obj -> V2 Float
proj v (Cord [x,y,z]) = (\[i,j] -> V2 i j) 
                      $ take 2 
                      $ toList
                      $ metric v * Data.Matrix.transpose (fromList 1 3 [y,-z,x])

-- computes the scalar product of the origin-centroid vector of an object with
-- the line of sight of the camera. Used in `seenFrom`.
closeness :: (Float,Float) -> [Float] -> Float
closeness (p,w) [x, y, z] = scalarProduct (toBaseline p w) [x,y,z]
  where toBaseline :: Float -> Float -> [Float] -- pitch, yaw in degrees
        toBaseline p w = [ cos theta * sin phi
                         , sin theta * sin phi
                         , cos phi
                         ]
          where theta = w * c
                phi   = (90-p) * c
                c     = pi / 180;

-- extremely barebones scalarProduct function. 
scalarProduct u v = dot v (unit u)
  where dot a b = sum $ zipWith (*) a b
        unit n  = map (/ norm n) n
        norm    = sqrt . sum . map (^2)

-- this isn't really the centroid, it's the average of the points in a figure.
centroid :: Obj -> [Float]
centroid (Cord coord         ) = avgPts [coord]
centroid (Edge pts           ) = avgPts pts
centroid (Face pts           ) = avgPts pts

avgPts :: [[Float]] -> [Float]
avgPts = map (\xs -> realToFrac (sum xs) / genericLength xs)
       . Data.List.transpose

-- render <width> <height> <zoom> <drawing> returns an Image.
-- It's useful to be able to define the scale independently here, to avoid
-- having to resize an entire World.
render :: Int -> Int -> Float -> Drawing PixelRGBA8 () -> Image PixelRGBA8
render x y s d = renderDrawing x y (PixelRGBA8 255 255 255 255)
               $ withTransformation ( translate (V2 (fromIntegral x / 2)
                                                    (fromIntegral y / 2)
                                                ) <> scale s s
                                    ) d
