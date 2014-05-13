module Main (main) where

--------------------------------------------------------------------------------

import qualified Octree as O
import Vec3D

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let a = O.emptyOctree (Vec3D (0, 0, 0)) 10 :: O.Octree Int
        b = O.splitTree a
        c = O.insert b (5, Vec3D (1, 2, 3))
    putStrLn $ show c
