module Main where

import Cornea
import Codec.Picture       (PixelRGBA8 (..), writePng)

-- Extremely basic coordinate axis system, originally used for calibration.
axes :: World
axes = [ ( Edge [[0,0,0], [15, 0, 0]] , mark (PixelRGBA8 255 100 100 255) 1)
       , ( Edge [[0,0,0], [ 0,15, 0]] , mark (PixelRGBA8 100 255 100 255) 1)
       , ( Edge [[0,0,0], [ 0, 0,15]] , mark (PixelRGBA8 100 100 255 255) 1)
       ]

-- Pretty simple example, to illustrate coordinates and edges.
ex01 :: World
ex01 = [ ( Cord p, solid (PixelRGBA8 0 0 0 255) ) | p <- pts ]
    ++ [ ( Edge [pa,pb], mark (PixelRGBA8 0 0 0 10) 1 ) | pa <- pts, pb <- pts ]
  where pts = [ [x,y,z] | x <- [-w,w], y <- [-w,w], z <- [-w,w] ]
        w = 10

-- More contrived example, to illustrate points, lines, and faces along and
-- within an icosahedron.
--
-- Cornea.hs doesn't handle intersecting faces well, so we draw these
-- rectangles piecewise by quadrant. Some errors can be seen where edges and
-- faces overlap.
ex02 :: World
ex02 = [ ( Cord p, solid (PixelRGBA8 0 0 0 255) ) | p <- pts ] 
    ++ [ ( Edge [pa,pb], mark (PixelRGBA8 0 0 0 20) 1 ) | pa <- pts, pb <- pts ]
    ++ map ( \(c,(a,b)) 
                -> ( Face [[a*0,0,b*0], [a*p,0,b*0], [a*p,0,b*s], [a*0,0,b*s]]
                   , solid (PixelRGBA8 c 255 100 255) 
                   ) 
           ) (zip cols loop)
    ++ map ( \(c,(a,b)) 
               -> ( Face [[a*0,b*0,0], [a*p,b*0,0], [a*p,b*s,0], [a*0,b*s,0]]
                  , solid (PixelRGBA8 100 c 255 255) 
                  ) 
           ) (zip cols loop)
    ++ map ( \(c,(a,b)) 
               -> ( Face [[0,b*0,a*0], [0,b*0,a*p], [0,b*s,a*p], [0,b*s,a*0]]
                  , solid (PixelRGBA8 255 c 100 255) 
                  ) 
           ) (zip cols loop)
  where pts  = [ [0, y, z] | y <- ss, z <- ps ]
            ++ [ [x, y, 0] | x <- ss, y <- ps ]
            ++ [ [x, 0, z] | x <- ps, z <- ss ]
        ps   = [-p,p]
        ss   = [-s,s]
        p    = s * (1 + sqrt 5) / 2
        s    = 20
        cols = [100,120,140,160]; loop = [(-1,-1),(1,-1),(1,1),(-1,1)]

-- to use, call 
-- writePng <filename> $ render <width> <height> <zoom> 
--                     $ <model> `seenFrom` <viewpoint>
main :: IO ()
main = do
  writePng "/tmp/axes.png" $ render 500 500 10 $ axes `seenFrom` isometric
  writePng "/tmp/ex01.png" $ render 500 500 10 $ ex01 `seenFrom` isometric
  writePng "/tmp/ex02.png" $ render 500 500  5 $ ex02 `seenFrom` (20,45)
