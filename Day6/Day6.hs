import qualified Data.HashSet as HS 
import qualified Data.HashMap.Strict as HM 

getStart :: HM.HashMap (Int, Int) Char -> Char -> (Int, Int)
getStart m s = HM.foldrWithKey (\k v acc -> if v == s then k else acc) (0,0) m

directions :: [(Int, Int)]
directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

addDirection :: (Int, Int) -> (Int, Int) -> (Int, Int)
addDirection (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

looper :: (Int, Int) -> Int -> HM.HashMap(Int, Int) Char -> HS.HashSet(Int, (Int, Int)) -> (Int, HS.HashSet(Int, (Int, Int)))
looper current direction grid seenList = do
    let newDirection = addDirection current (directions !! direction)
    let newSet = HS.insert (direction, current) seenList
    if HS.member (direction, current) seenList then (1, seenList)
    else case HM.lookup newDirection grid of
            Just '#' -> looper current ((direction + 1) `mod` 4) grid newSet
            Just v -> looper newDirection direction grid newSet 
            Nothing -> (0, seenList)

main :: IO ()
main = do
    content <- readFile "input1.txt"
    let m = foldl (\acc1 (k1, v) -> 
                foldl (\acc2 (k2, v2) -> HM.insert (k1, k2) v2 acc2) acc1 (zip [0..] v)
            ) HM.empty (zip [0..] (lines content))
    let start = getStart m '^'
    let visited = HS.toList (HS.map snd (snd (looper start 0 m HS.empty)))
    print $ length visited + 1
    print $ sum (map (\center -> fst (looper start 0 (HM.insert center '#' m) HS.empty)) visited)