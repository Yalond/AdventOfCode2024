import Data.List (sort, group, intersect)

splitRowInt :: String -> (Int, Int)
splitRowInt str = 
    let (first, rest) = span(/= ' ') str
    in (read first, read (drop 1 rest))

getCount :: (Int, [Int]) -> Int
getCount (x, collection) = 
    if null collection || x < head collection then 0
    else fromEnum(x == head collection) + getCount(x, tail collection)

getSum :: ([Int], [Int]) -> Int
getSum (a, b) = 
    if null a then 0
    else (head a * getCount(head a, b)) + getSum(tail a, b)

main :: IO()
main = do

    content <- readFile "input1.txt"
    let fileLines = lines content

    let (first, second) = unzip (map splitRowInt fileLines)

    let s1 = sort first
    let s2 = sort second 

    print (sum (zipWith (\a b -> abs(a - b) ) s1 s2))
    print(getSum(s1, s2))

