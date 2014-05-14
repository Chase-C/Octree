module Main (main) where

--------------------------------------------------------------------------------

import qualified Octree as O
import Vec3D

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let a = O.emptyOctree (Vec3D (0, 0, 0)) 10 :: O.Octree Int
        b = O.insert a (1, Vec3D (  1,    2,    3))
        c = O.insert b (2, Vec3D ((-1), (-2), (-3)))
        d = O.insert c (3, Vec3D ((-9),   2,    3))
        e = O.insert d (4, Vec3D (  5,  (-2),   5))
        f = O.insert e (5, Vec3D (  1,    2,  (-3)))
        g = O.splitTree f
        h = O.insert g (6, Vec3D ((-5), (-5),   4))
        i = O.insert h (7, Vec3D (  5,  (-4), (-5)))
        j = O.insert i (8, Vec3D ((-6),   5,  (-5)))
        k = O.insert j (9, Vec3D (  5,    6,    5))
        l = O.insert k (0, Vec3D ((-5), (-5), (-6)))
    putStrLn $ O.prettyPrint l
