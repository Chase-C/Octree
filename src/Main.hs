module Main (main) where

--------------------------------------------------------------------------------

import System.Random
import System.TimeIt
import qualified Octree    as O
import qualified Data.List as L
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

getRandom :: (Random a) => a -> a -> IO a
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

main :: IO ()
main = do
    testSet <- mapM (\n -> do
                           x <- getRandom (-50) 50 :: IO Float
                           y <- getRandom (-50) 50 :: IO Float
                           z <- getRandom (-50) 50 :: IO Float
                           return (n, Vec3D (x, y, z))) ([1..20000] :: [Int])
    let tree = O.splitWith (O.insertList (O.emptyOctree (Vec3D (0, 0, 0)) 128 :: O.Octree Int) testSet) ((> 8) . O.count)
        vec = Vec3D (-8, -8, 8)

    --putStrLn $ O.prettyPrint tree
    timeIt $ putStrLn $ show $ O.kNearestNeighbors tree vec 5 8
    timeIt $ putStrLn $ show $ O.getRadiusObjects  tree vec   8

    timeIt $ putStrLn $ show $ take 7 $ L.sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) $
        map (\(o, v) -> (o, vLen $ vSub v vec)) $ O.flattenTree tree
