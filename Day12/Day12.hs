{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Data.Sequence as Seq
import  Data.List (groupBy, sortOn)

data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

type Grid a = Map.Map Point a

data GridMap a = GridMap
  { grid :: Grid a
  , rows :: Int
  , cols :: Int
  } deriving (Show, Eq)

buildGrid :: (Char -> a) -> [String] -> Grid a
buildGrid f rows = Map.fromList [ (Point r c, f ch)
                              | (r, row) <- zip [0..] rows
                              , (c, ch)  <- zip [0..] row
                              ]

buildGridMap :: (Char -> a) -> [String] -> GridMap a
buildGridMap f rows = GridMap (buildGrid f rows) (length rows) (length (head rows))

add :: Point -> Point -> Point
add p1 p2 = Point (x p1 + x p2) (y p1 + y p2)

pointInRange :: GridMap a -> Point -> Bool
pointInRange gridMap p = x p >= 0 && x p < cols gridMap  && y p >= 0 && y p < rows gridMap

neighbourOffsets :: [Point]
neighbourOffsets = [Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)]

getNeighbours :: Point -> (Point -> Bool) -> GridMap a ->  [Point]
getNeighbours point f gridMap =
  filter (\p -> pointInRange gridMap p && f p) (map (`add` point) neighbourOffsets)

getNeighbours2 :: Point -> (Point -> Bool) -> Map.Map Point a ->  [Point]
getNeighbours2 point f gridMap =
  filter f (map (`add` point) neighbourOffsets)

getNeighboursPerm :: Set.Set Point -> Point -> [Point]
getNeighboursPerm region point = filter (\p -> not (Set.member p region )) (map (`add` point) neighbourOffsets)

-- retuns a list of sides of this point which need fences
getSidesWithFences :: Set.Set Point -> Point -> [(Point, Int)]
getSidesWithFences region point = foldl (\acc (p, side) ->
        if not $ Set.member p region then (point, side) : acc
        else acc) [] (zip (map (`add` point) neighbourOffsets) [0..])

lookupGrid :: Point -> a -> GridMap a -> a
lookupGrid point defaultValue gridMap = fromMaybe defaultValue (Map.lookup point (grid gridMap))

lookupMap :: Point -> Int -> Map.Map Point Int -> Int
lookupMap point defaultValue gridMap = fromMaybe defaultValue (Map.lookup point gridMap)

getRegion :: Point -> Char -> GridMap Char -> Set.Set Point -> Set.Set Point
getRegion currentPoint identifier gridMap seenList = do
  let updatedSeenList = Set.insert currentPoint seenList

  if Set.member currentPoint seenList then seenList
  else do
    let neighbours = getNeighbours currentPoint (\point -> lookupGrid point '0' gridMap == identifier) gridMap
    foldl (\accSeen p -> Set.union accSeen (getRegion p identifier gridMap accSeen)) updatedSeenList neighbours


getAllregions :: GridMap Char -> [Set.Set Point]
getAllregions gridMap = do
  let mt = Map.toList $ grid gridMap
  let (globalSet,regionSets) = foldl (\(gs, ls) (point, target) ->
        if Set.member point gs then (gs, ls)
        else let region = getRegion point target gridMap Set.empty in (Set.union gs region, region : ls)
        ) (Set.empty, []) mt
  regionSets


calculatePerimiter :: Set.Set Point -> Int
calculatePerimiter s = do
  let perim = concatMap (getNeighboursPerm s) (Set.toList s)
  length perim * Set.size s

buildPaths :: Point -> Int -> Map.Map Point Int -> Set.Set Point -> Set.Set Point
buildPaths currentPoint identifier grid seenList = do
  let updatedSeenList = Set.insert currentPoint seenList

  if Set.member currentPoint seenList then seenList
  else do
    let neighbours = getNeighbours2 currentPoint (\point -> lookupMap point (-1) grid == identifier) grid
    foldl (\accSeen p -> Set.union accSeen (buildPaths p identifier grid accSeen)) updatedSeenList neighbours

calculateSidesPerSide :: [(Point, Int)] -> Int
calculateSidesPerSide sides = do
  let s = Set.fromList (map fst sides)
  let sm = Map.fromList sides
  let (globalSet,regionSets) = foldl (\(gs, ls) (point, target) ->
        if Set.member (point, target) gs then (gs, ls)
        else let region = Set.map (\p -> (p, target)) (buildPaths point target sm Set.empty) in (Set.union gs region, region : ls
        )) (Set.empty, []) sides

  length regionSets

calculateSides :: Set.Set Point -> Int
calculateSides s = length s * sum (map calculateSidesPerSide pointsPerSide)
  where 
    sides = concatMap (getSidesWithFences s) (Set.toList s)
    pointsPerSide = groupBy (\a b -> snd a == snd b) (sortOn snd sides)

main :: IO()
main = do
  content <- readFile "input1.txt"

  let gridMap = buildGridMap id $ lines content
  let regions = getAllregions gridMap

  print $ sum $ map calculatePerimiter regions
  print $ sum $ map calculateSides regions
