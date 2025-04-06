import Data.List.Split (splitOn)

ci :: Int -> Int -> Int
ci a b = read (show a ++ show b)

calculate :: Int -> [Int -> Int -> Int] -> [Int] -> Bool
calculate t s arr 
    | null arr || head arr > t = False 
    | length arr == 1 = t - head arr == 0
    | otherwise =
        let p1 = head arr
            p2 = arr !! 1
            res = drop 2 arr
        in any (\f -> calculate t s (f p1 p2: res)) s

check :: String -> [Int -> Int -> Int] -> Int
check content part = foldl (\acc row -> do
    let (a, b) = break (== ':') row
    acc + if calculate (read a) part (map read (splitOn " " (drop 2 b))) then read a else 0
    ) 0 (lines content)

main :: IO()
main = do
    content <- readFile "input1.txt"
    print $ check content [(+), (*)]
    print $ check content [(+), (*), ci]