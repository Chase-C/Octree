module Main (main) where

--------------------------------------------------------------------------------

import System.Random
--import System.TimeIt
import Control.Monad
import Control.Applicative

import qualified Octree    as O
import qualified Data.List as L

import Vec3D

--------------------------------------------------------------------------------

getRandom :: (Random a) => a -> a -> IO a
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

randVec :: Float -> Float -> IO Vec3D
randVec l u = do
    x <- getRandom l u :: IO Float
    y <- getRandom l u :: IO Float
    z <- getRandom l u :: IO Float
    return $ Vec3D (x, y, z)

testFunc :: (Show a) => (O.Octree a -> Vec3D -> Int -> Float -> [(a, Float)]) -> O.Octree a -> Int -> Float -> IO Int
testFunc func tree k radius = do
    vec <- randVec (-50) 50
    return $ length $ func tree vec k radius

main :: IO ()
main = do
    testSet <- mapM (\n -> do
                           vec <- randVec (-50) 50
                           return (n, vec)) ([1..10000] :: [Int])
    let tree = O.splitWith (O.insertList (O.emptyOctree (Vec3D (0, 0, 0)) 128 :: O.Octree Int) testSet) ((> 8) . O.count)

    --timeIt $ putStrLn $ show $ O.count tree
    y <- foldM (\acc _ -> (+) <$> (return acc) <*> testFunc O.kNearestNeighbors  tree 5 8) 0 ([1..10000] :: [Int])
    putStrLn $ show y
    putStrLn $ show $ O.kNearestNeighbors tree zeroVec 5 8
    putStrLn $ show $ O.getRadiusObjects  tree zeroVec   8

    putStrLn $ show $ take 5 $ L.sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) $
        map (\(o, v) -> (o, vLen $ vSub v zeroVec)) $ O.flattenTree tree
