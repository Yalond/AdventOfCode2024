import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

type Grid = Map.Map Point Int

data GridMap = GridMap
  { grid :: Grid
  , rows :: Int
  , cols :: Int
  } deriving (Show, Eq)

add :: Point -> Point -> Point
add p1 p2 = Point (x p1 + x p2) (y p1 + y p2)

pointInRange :: GridMap -> Point -> Bool
pointInRange gridMap p = x p >= 0 && x p < cols gridMap  && y p >= 0 && y p < rows gridMap

neighbourOffsets :: [Point]
neighbourOffsets = [Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)]

getNeighbours :: Point -> Int -> GridMap ->  [Point]
getNeighbours point value gridMap = do
  filter (\p -> pointInRange gridMap p && fromMaybe (-1) (Map.lookup p (grid gridMap)) == (value + 1) ) (map (`add` point) neighbourOffsets)


scoreTrailhead :: Point -> GridMap -> Set.Set Point -> Set.Set Point
scoreTrailhead point gridMap seenList = do
  let currentValue = fromMaybe (-1) (Map.lookup point (grid gridMap))
  let updatedSeenList = Set.insert point seenList

  if Set.member point seenList then seenList
  else if currentValue == 9 then updatedSeenList
  else do
    let neighbours = getNeighbours point currentValue gridMap
    foldr (Set.union . (\p -> scoreTrailhead p gridMap updatedSeenList)) Set.empty neighbours

scoreTrailhead2 :: Point -> GridMap -> [Point] -> Set.Set [Point] -> Set.Set [Point]
scoreTrailhead2 point gridMap currentPath seenList = do
  let currentValue = fromMaybe (-1) (Map.lookup point (grid gridMap))
  let updatedSeenList = Set.insert (point : currentPath) seenList

  if Set.member (point : currentPath) seenList then seenList
  else if currentValue == 9 then updatedSeenList
  else do
    let neighbours = getNeighbours point currentValue gridMap
    foldr (Set.union . (\p -> scoreTrailhead2 p gridMap (point : currentPath) updatedSeenList)) Set.empty neighbours

main :: IO()
main = do
  content <- readFile "input1.txt"

  let rows = lines content

  let m = foldl (\acc1 (k1, v) ->
              foldl (\acc2 (k2, v2) -> Map.insert (Point k1 k2) (digitToInt v2) acc2) acc1 (zip [0..] v)
          ) Map.empty (zip [0..] rows) :: Grid

  let gridMap = GridMap m (length rows) (length (head rows))

  let trailheads = map fst (filter (\(key, value) -> value == 0) (Map.toList m))
  let targets = Set.fromList (map fst (filter (\(key, value) -> value == 9) (Map.toList m)))

  let pointLists = map (\p -> scoreTrailhead p gridMap Set.empty) trailheads
  let pointLists2 = map (\p -> scoreTrailhead2 p gridMap [] Set.empty) trailheads

  print $ sum $ map (\pl -> length (Set.intersection pl targets)) pointLists
  print $ sum $ map (length . Set.filter (\xs -> length xs == 10)) pointLists2



