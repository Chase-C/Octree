module Main (main) where

--------------------------------------------------------------------------------

import qualified Octree as O
import Vec3D

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let a = O.emptyOctree (Vec3D (0, 0, 0)) 16 :: O.Octree Int
        b = O.insert a (1, Vec3D (  1,    2,    3))
        c = O.insert b (2, Vec3D ((-1), (-2), (-3)))
        d = O.insert c (3, Vec3D ((-9),   2,    3))
        e = O.insert d (4, Vec3D (  5,  (-2),   5))
        f = O.insert e (5, Vec3D (  1,    2,  (-3)))
        --g = O.splitWith f ((> 8) . O.count)
    putStrLn $ O.prettyPrint f

    let h = O.insert f (6, Vec3D ((-5), (-5),   4))
        i = O.insert h (7, Vec3D (  5,  (-4), (-5)))
        j = O.insert i (8, Vec3D ((-6),   5,  (-5)))
        k = O.insert j (9, Vec3D (  5,    6,    5))
        l = O.insert k (0, Vec3D ((-5), (-5), (-6)))
        --m = O.splitWith l ((> 8) . O.count)
    putStrLn $ O.prettyPrint l

    let n = O.insert l (11, Vec3D (2, 2, 2))
        o = O.insert n (12, Vec3D (6, 6, 6))
        p = O.insert o (13, Vec3D (6, 2, 2))
        q = O.insert p (14, Vec3D (2, 6, 2))
        r = O.insert q (15, Vec3D (2, 2, 6))
        s = O.insert r (16, Vec3D (6, 6, 2))
        t = O.insert s (17, Vec3D (2, 6, 6))
    putStrLn $ O.prettyPrint t

    let u = O.splitWith t ((>8) . O.count)
    putStrLn $ O.prettyPrint u
    putStrLn $ show $ O.getNearObjects u (Vec3D (6, 6, 6))
