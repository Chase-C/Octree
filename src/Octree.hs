module Octree where

---------------------------------------------------------

import Data.Bits
import qualified Data.List as L

import Vec3D

---------------------------------------------------------

data Octree a = Node
                  { center :: Vec3D
                  , len    :: Float
                  , ftr, ftl, fbr, fbl, btr, btl, bbr, bbl :: Octree a
                  } -- front, back, top, bottom, right, left
              | Leaf
                  { center  :: Vec3D
                  , len     :: Float
                  , objects :: [(a, Vec3D)]
                  }
                deriving (Show)

data Octant = FTR | FTL | FBR | FBL | BTR | BTL | BBR | BBL deriving (Show, Eq, Ord, Enum)

---------------------------------------------------------

emptyOctree :: Vec3D -> Float -> Octree a
emptyOctree c l = Leaf c l []

---------------------------------------------------------

octreeFold :: (a -> b -> a) -> a -> Octree b -> a
octreeFold func i (Node _ _ a b c d e f g h) = octreeFold func p h
    where j = octreeFold func i a
          k = octreeFold func j b
          l = octreeFold func k c
          m = octreeFold func l d
          n = octreeFold func m e
          o = octreeFold func n f
          p = octreeFold func o g
octreeFold func i (Leaf _ _ objs) = foldl (\acc x -> func acc $ fst x) i objs

octreeFold' :: (a -> (b, Vec3D) -> a) -> a -> Octree b -> a
octreeFold' func i (Node _ _ a b c d e f g h) = octreeFold' func p h
    where j = octreeFold' func i a
          k = octreeFold' func j b
          l = octreeFold' func k c
          m = octreeFold' func l d
          n = octreeFold' func m e
          o = octreeFold' func n f
          p = octreeFold' func o g
octreeFold' func i (Leaf _ _ objs) = foldl (\acc x -> func acc x) i objs

---------------------------------------------------------

prettyPrint :: (Show a) => Octree a -> String
prettyPrint (Node cen l a b c d e f g h) = "Node {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++ "\n" ++
    (concat $ L.intersperse "\n" $ map prettyPrint [a, b, c, d, e, f, g, h]) ++ "\n}"
prettyPrint (Leaf cen l objs) = "Leaf {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++ "\n\t" ++
    (concat $ L.intersperse "\n\t" $ map show objs) ++ "\n}"

---------------------------------------------------------

getOctant :: Vec3D -> Vec3D -> Octant
getOctant (Vec3D (cx, cy, cz)) (Vec3D (px, py, pz)) = toEnum $ (fromEnum right) + (2 * fromEnum top) + (4 * fromEnum front)
    where front = pz < cz
          top   = py < cy
          right = px < cx

getSubtree :: Octree a -> Octant -> Octree a
getSubtree (Node _ _ a b c d e f g h) octant =
    case octant of
      FTR -> a
      FTL -> b
      FBR -> c
      FBL -> d
      BTR -> e
      BTL -> f
      BBR -> g
      BBL -> h
getSubtree tree _ = tree

replaceSubtree :: Octree a -> Octant -> Octree a -> Octree a
replaceSubtree (Node cen l a b c d e f g h) octant subtree =
    case octant of
      FTR -> Node cen l subtree b c d e f g h
      FTL -> Node cen l a subtree c d e f g h
      FBR -> Node cen l a b subtree d e f g h
      FBL -> Node cen l a b c subtree e f g h                                
      BTR -> Node cen l a b c d subtree f g h
      BTL -> Node cen l a b c d e subtree g h
      BBR -> Node cen l a b c d e f subtree h
      BBL -> Node cen l a b c d e f g subtree
replaceSubtree tree _ _ = tree

flattenTree :: Octree a -> [(a, Vec3D)]
flattenTree tree = octreeFold' (\xs obj -> obj:xs) [] tree

count :: Octree a -> Int
count = octreeFold (\acc _ -> acc + 1) 0

insert :: Octree a -> (a, Vec3D) -> Octree a
insert (Leaf cen l xs) obj = Leaf cen l $ obj:xs
insert node            obj = replaceSubtree node octant $ insert (getSubtree node octant) obj
    where octant = getOctant (center node) (snd obj)

insertList :: Octree a -> [(a, Vec3D)] -> Octree a
insertList = foldl insert

splitTree :: Octree a -> Octree a
splitTree (Leaf c@(Vec3D (cx, cy, cz)) l objs) = foldl insert tree objs
    where tree = Node
                   { center = c
                   , len = l
                   , ftr = et rx ty fz, ftl = et lx ty fz
                   , fbr = et rx by fz, fbl = et lx by fz
                   , btr = et rx ty bz, btl = et lx ty bz
                   , bbr = et rx by bz, bbl = et lx by bz
                   }
          et x y z = emptyOctree (Vec3D (x, y, z)) hl
          hl       = l / 2
          rx       = cx + hl
          lx       = cx - hl
          ty       = cy + hl
          by       = cy - hl
          fz       = cz + hl
          bz       = cz - hl
splitTree tree = tree

splitWith :: Octree a -> (Octree a -> Bool) -> Octree a
splitWith (Node cen len i j k l m n o p) f = Node cen len (s i) (s j) (s k) (s l) (s m) (s n) (s o) (s p)
    where s tree = splitWith tree f
splitWith tree func 
    | func tree = splitWith (splitTree tree) func
    | otherwise = tree

getNearObjects :: Octree a -> Vec3D -> [a]
getNearObjects (Leaf _ _ objs) _ = map fst objs
getNearObjects node pos          = getNearObjects subtree pos
    where subtree = getSubtree node $ getOctant (center node) pos

xOppOctant, yOppOctant, zOppOctant :: Octant -> Octant
xOppOctant octant = toEnum $ xor (fromEnum octant) 1
yOppOctant octant = toEnum $ xor (fromEnum octant) 2
zOppOctant octant = toEnum $ xor (fromEnum octant) 4

getRadiusObjects :: Octree a -> Vec3D -> Float -> [a]
getRadiusObjects (Leaf _ _ objs) pos r = map fst $ filter (\obj -> (r * r) > (vSqLen $ vSub pos $ snd obj)) objs
getRadiusObjects node pos r = concat . (map (\t -> getRadiusObjects t pos r)) $ intersectingSubtrees node pos r

-- Return True iff the sphere around the given position exceeds the bounds of
-- the given Octree.
{-# INLINE inBounds #-}
inBounds :: Octree a -> Vec3D -> Float -> Bool
inBounds tree pos rad = lX && lY && lZ && uX && uY && uZ
    where Vec3D (x, y, z) = vSub pos $ center tree
          hl = len tree / 2
          lX = -hl < x - rad
          lY = -hl < y - rad
          lZ = -hl < z - rad
          uX =  hl > x + rad
          uY =  hl > y + rad
          uZ =  hl > z + rad

-- Return a list of the subtrees intersecting with the given bounding sphere
-- Note: The last subtree in the list is always the the one containing the point
{-# INLINE intersectingSubtrees #-}
intersectingSubtrees :: Octree a -> Vec3D -> Float -> [Octree a]
intersectingSubtrees l@(Leaf _ _ _) _ _ = return l
intersectingSubtrees node p@(Vec3D (px, py, pz)) rad = map (getSubtree node) octants
    where octant  = getOctant c p
          octants = if rad > abs (pz - cz) then foldr (\o zs -> (zOppOctant o):o:zs) [] tmpY else tmpY
          tmpY    = if rad > abs (py - cy) then foldr (\o ys -> (yOppOctant o):o:ys) [] tmpX else tmpX
          tmpX    = if rad > abs (px - cx) then (xOppOctant octant):[octant] else [octant]
          c@(Vec3D (cx, cy, cz)) = center node

kNearestNeighbors :: Octree a -> Vec3D -> Int -> Float -> [(a, Float)]
kNearestNeighbors (Leaf _ _ objs) pos k maxR =
    take k $ L.sortBy sortByDist $ filter filtFunc $ map (getObjDist pos) objs
    where filtFunc (_, rad)        = rad < maxR
kNearestNeighbors node pos k maxR
    | inBounds subtree pos topR && length nearest >= k = nearest
    | otherwise = take k $ foldl combineNeighbors nearest $ getOtherNeighbors node pos k topR
    where subtree = getSubtree node (getOctant (center node) pos)
          nearest = kNearestNeighbors subtree pos k maxR
          topR    = if length nearest >= k then snd $ last nearest else maxR

{-# INLINE getOtherNeighbors #-}
getOtherNeighbors :: Octree a -> Vec3D -> Int -> Float -> [[(a, Float)]]
getOtherNeighbors tree pos k rad =
    map (\t -> kNearestNeighbors t pos k rad) $ init $ intersectingSubtrees tree pos rad

{-# INLINE combineNeighbors #-}
combineNeighbors :: [(a, Float)] -> [(a, Float)] -> [(a, Float)]
combineNeighbors xs [] = xs
combineNeighbors [] ys = ys
combineNeighbors (x@(_, rx):xs) (y@(_, ry):ys) =
    if rx > ry
      then y : combineNeighbors (x:xs) ys
      else x : combineNeighbors xs (y:ys)

{-# INLINE getObjDist #-}
getObjDist :: Vec3D -> (a, Vec3D) -> (a, Float)
getObjDist pos (obj, vec) = (obj, vDist vec pos)

{-# INLINE sortByDist #-}
sortByDist :: (a, Float) -> (a, Float) -> Ordering
sortByDist (_, r1) (_, r2) = r1 `compare` r2
