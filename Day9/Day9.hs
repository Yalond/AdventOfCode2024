import Data.Char (digitToInt, chr)
import Data.Tree (flatten)
import Data.IntMap (split)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.List

iter :: [Int] -> [Int] -> Int -> [Int]
iter disk reversed len
  | len == 0 = []
  | head disk == -1 = head reversed : iter (tail disk) (tail reversed) (len - 1)
  | otherwise = head disk : iter (tail disk) reversed (len - 1)


replaceFirst :: [a] -> [a] -> [a]
replaceFirst [] ys = ys
replaceFirst (x:xs) (y:ys) = x : replaceFirst xs ys


reinsert :: Int -> Int -> [Int] -> [Int]
reinsert targetLength targetValue xs
  | null xs = []
  | (head xs == -1) && length headList >= targetLength = do
    let inserted = replaceFirst (Prelude.replicate targetLength targetValue) headList
    inserted ++ map (\x -> if x == targetValue then -1 else x) tailList
  | (head xs == targetValue) && length headList >= targetLength = headList ++ tailList
  | otherwise = headList ++ reinsert targetLength targetValue tailList
    where
      (headList, tailList) = span (\a -> a == head xs) xs


iterArr :: Int -> Map.Map Int Int -> [Int] -> [Int]
iterArr index m xs =
  if index == 0 then xs
  else do
    let len = case Map.lookup index m of
          Just b -> b
          Nothing -> 0
    let res = reinsert len index xs 
    iterArr (index - 1) m res


main :: IO()
main = do
  content <- readFile "input1.txt"

  let disk = concatMap (\(index, value) -> Prelude.replicate (digitToInt value) (if even index then index `div` 2 else -1)) (zip [0..] content)
  print $ length disk

  let reversed = reverse (filter (\x -> x /= -1) disk)
  print $ sum $ zipWith (*) (iter disk reversed (length reversed)) [0..]
  print $ maximum disk

  let m = foldl (\m_acc v -> Map.insert (head v) (length v) m_acc) Map.empty (group disk)
  let r1 = iterArr (maximum disk) m disk
  print $ sum $ zipWith (\ a b -> (if a == - 1 then 0 else a * b)) r1 [0..]
