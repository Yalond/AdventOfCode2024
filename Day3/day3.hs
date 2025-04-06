import Text.Regex.TDFA 
import Data.List.Split

--extractDigits :: String -> [String]
--extractDigits input = input =~ "[0-9]+" :: [String]

--getAllTExtMatches (a =~ b) :: [String]


cleaner :: String -> Int
cleaner s = do 
    let t = (take (length s - 1) (drop 4 s))
    let (a b) = splitOn "," t
    (read a) * (read b)
    

main :: IO()
main = do
    content <- readFile "input1.txt"
    let rows = lines content 
    let term = "mul\\([0-9]*\\,[0-9]*\\)"
    let raw = map (\x -> getAllTextMatches (x =~ term)) rows :: [[String]]
    let clean = map (\r -> map cleaner r) raw
    print $ sum clean
    --print $ extractDigits a