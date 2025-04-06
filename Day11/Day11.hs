import Data.List.Split
import qualified Data.Map as Map

splitStone :: Int -> [Int]
splitStone stone = do
  let s = show stone
  let size = length s `div` 2
  [read (take size s), read (drop size s) ]

countMemoized :: Int -> Int -> Map.Map (Int, Int) Int -> (Map.Map (Int, Int) Int, Int)
countMemoized index stone mem = do

  let (m, res) = (case Map.lookup (index, stone) mem of
        Just x -> (mem, x)
        Nothing -> if index == 0 then (mem, 1)
                    else if stone == 0 then countMemoized (index - 1) 1 mem
                    else if even (length (show stone)) then do
                      let r = splitStone stone
                      let (m1, v1) = countMemoized (index - 1) (head r) mem
                      let (m2, v2) = countMemoized (index - 1) (r !! 1) m1
                      (m2, v1 + v2)

                    else countMemoized (index - 1) (stone * 2024) mem)

  (Map.insert (index, stone) res m, res)


main :: IO()
main = do
  content <- readFile "input1.txt"
  let initial = map read (splitOn " " content)

  print $ snd $ foldl (\(m, total) stone -> let (nm, ni) = countMemoized 25 stone m in (Map.union m nm, ni + total)) (Map.empty, 0) initial
  print $ snd $ foldl (\(m, total) stone -> let (nm, ni) = countMemoized 75 stone m in (Map.union m nm, ni + total)) (Map.empty, 0) initial

