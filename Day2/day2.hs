checkRow :: [Int] -> Bool
checkRow xs = do
    let g = zipWith (-) xs (tail xs);
    ((&&) <$> all ( > 0) <*> all ( < 4)) (map abs g) && ( all (> 0) g || all (< 0) g)

checkPermute :: [Int] -> Bool
checkPermute xs = 
    or [checkRow (take i xs ++ drop (i + 1) xs) | i <- [0 .. length xs - 1]]

main :: IO()
main = do
    content <- readFile "input1.txt"
    let segments = map (map read . words) (lines content)
    print (length (filter checkRow segments))
    print (length (filter checkPermute segments))
