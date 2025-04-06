import qualified Data.Map as Map
import qualified Data.Set as Set

insertMulti :: Ord a => a -> b -> Map.Map a [b] -> Map.Map a [b]
insertMulti key value = Map.insertWith (++) key [value]

addToMap :: Int -> String -> Map.Map Char [(Int, Int)] -> Map.Map Char [(Int, Int)]
addToMap rowIndex row m =
    foldl (\mb (colIndex, c) -> if c /= '.' then insertMulti c (rowIndex, colIndex) mb else mb) m (zip [0..] row)

inRange :: Int -> Int -> (Int, Int) -> Bool
inRange row col (r, c) = r >= 0 && r < row && c >= 0 && c < col

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints  (p1r, p1c) (p2r, p2c) = (p1r + (p1r - p2r), p1c + (p1c - p2c))

getDistances :: ((Int, Int) -> Bool) -> Bool -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getDistances checker shouldRecurse p1 p2
    | p1 == p2 = []
    | shouldRecurse && checker p1 = p1 : getDistances checker shouldRecurse prev p1
    | not shouldRecurse && checker prev = [prev]
    | otherwise = []
    where prev = addPoints p1 p2
 
permute :: Int -> ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
permute index f xs
  | (index > (length xs - 1)) || length xs <= 1 = []
  | otherwise = concatMap (f (xs !! index)) xs ++ permute (index + 1) f xs

main :: IO()
main = do
    content <- readFile "input1.txt"
    let rows = lines content

    let checker = inRange (length rows) ((length . head) rows)

    let items = Map.elems $ foldl (\m (i, r) -> addToMap i r m) Map.empty (zip [0..] rows)

    print $ Set.size $ Set.fromList $ concatMap (permute 0 (getDistances checker False)) items
    print $ Set.size $ Set.fromList $ concatMap (permute 0 (getDistances checker True)) items