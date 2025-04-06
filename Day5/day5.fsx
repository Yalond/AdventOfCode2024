open System.IO

let checkSingleRule (row: int array) (before: int, after: int) =
    if (Array.contains before row) && (Array.contains after row) then 
        (Array.findIndex ((=) before) row) < (Array.findIndex ((=) after) row) 
    else true

let checkRow rules row  = 
    Array.fold (fun acc rule -> acc && (checkSingleRule row rule)) true rules

let rec getDir (rules: array<int * int>) a b =
    let (ra, rb) = Array.head rules
    if ra = a && rb = b then -1
    elif ra = b && rb = a then 1
    else getDir (Array.tail rules) a b

let lines = File.ReadAllLines("input1.txt")

let i = Array.findIndex ((=) "") lines
let rows =  [|for r in lines[i+1..] -> r.Split(',') |> Array.map int|]
let rules = [|for r in lines[0..i-1] -> let p = r.Split('|') in (int p.[0], int p.[1])|]

let grabber = Array.fold (fun acc (arr: int array) -> acc + arr.[(Array.length arr)/2]) 0
printfn "%d" (grabber (Array.filter (checkRow rules) rows)) 
printfn "%d" (grabber ((Array.filter (not << (checkRow rules)) rows) |> 
                        Array.map (Array.sortWith (getDir rules))))
                        