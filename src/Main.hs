module Main (main) where

--------------------------------------------------------------------------------

import System.Random
import System.TimeIt
import Control.Monad
import Control.Applicative

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
                           return (n, vec)) ([1..50000] :: [Int])
    let tree = O.splitWith (O.insertList (O.emptyOctree (Vec3D (0, 0, 0)) 128 :: O.Octree Int) testSet) ((> 8) . O.count)

    timeIt $ putStrLn $ show $ O.count tree
    x <- foldM (\acc _ -> (+) <$> (return acc) <*> testFunc O._kNearestNeighbors tree 5 8) 0 ([1..10000] :: [Int])
    timeIt $ putStrLn $ "Inline - " ++ show x
    y <- foldM (\acc _ -> (+) <$> (return acc) <*> testFunc O.kNearestNeighbors  tree 5 8) 0 ([1..10000] :: [Int])
    timeIt $ putStrLn $ "Non-inline - " ++ show y
    z <- foldM (\acc _ -> (+) <$> (return acc) <*> testFunc O._kNearestNeighbors tree 5 8) 0 ([1..10000] :: [Int])
    timeIt $ putStrLn $ "Inline - " ++ show z
    t <- foldM (\acc _ -> (+) <$> (return acc) <*> testFunc O.kNearestNeighbors  tree 5 8) 0 ([1..10000] :: [Int])
    timeIt $ putStrLn $ "Non-inline - " ++ show t
    --timeIt $ putStrLn $ show $ O.getRadiusObjects   tree vec   8

    --timeIt $ putStrLn $ show $ take 7 $ L.sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) $
    --    map (\(o, v) -> (o, vLen $ vSub v vec)) $ O.flattenTree tree
