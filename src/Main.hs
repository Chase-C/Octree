module Main (main) where

--------------------------------------------------------------------------------

import qualified Octree as O
import Vec3D

--------------------------------------------------------------------------------

xs :: [(Int, Vec3D)]
xs = (1,  Vec3D (  1,    2,    3))
   : (2,  Vec3D ((-1), (-2), (-3)))
   : (3,  Vec3D ((-9),   2,    3))
   : (4,  Vec3D (  5,  (-2),   5))
   : (5,  Vec3D (  2,    6,    6))
   : (18, Vec3D (  1,    2,  (-3)))
   : (6,  Vec3D ((-5), (-5),   4))
   : (7,  Vec3D (  5,  (-4), (-5)))
   : (8,  Vec3D ((-6),   5,  (-5)))
   : (9,  Vec3D (  5,    6,    5))
   : (0,  Vec3D ((-5), (-5), (-6)))
   : (19, Vec3D (2, 2, 2))
   : (11, Vec3D (2, 2, 6))
   : (12, Vec3D (6, 6, 6))
   : (13, Vec3D (6, 2, 2))
   : (14, Vec3D (2, 6, 2))
   : []

tree :: O.Octree Int
tree = O.splitWith (O.insertList (O.emptyOctree (Vec3D (0, 0, 0)) 16 :: O.Octree Int) xs) ((> 8) . O.count)

main :: IO ()
main = do
    putStrLn $ O.prettyPrint tree
    putStrLn $ show $ O.kNearestNeighbors tree (Vec3D (8, 8, 8)) 5 10
    putStrLn $ show $ O.getRadiusObjects  tree (Vec3D (8, 8, 8))   6
