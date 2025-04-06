open System.IO

let term = [|"X";"M";"A";"S"|]

let logos = [|[|
    [|"M";".";"M"|]
    [|".";"A";"."|]
    [|"S";".";"S"|]
|]; [|
    [|"M";".";"S"|]
    [|".";"A";"."|]
    [|"M";".";"S"|]
|];[|
    [|"S";".";"M"|]
    [|".";"A";"."|]
    [|"S";".";"M"|]
|];[|
    [|"S";".";"S"|]
    [|".";"A";"."|]
    [|"M";".";"M"|]
|]|]

let createList (grid: string array array) (center: int array) (len: int) (x: int) (y: int) =
    [|for i in 0..((-) len 1) do grid[center[0] + i * x][center[1] + i * y]|]

let checkWindow (w: string array array) =
    let c = [|3;3|]
    let lg = createList w c 4
    let t = [|lg 0 1; lg 0 -1; lg 1 0; lg -1 0; lg 1 -1; lg -1 1; lg -1 -1; lg 1 1|]
    Array.length (Array.filter (fun x -> Array.forall2 (=) x term) t)

let checkLogo(w: string array array) (p: string array array) (o: int) (z: int)=
    w[o][o] = p[z][z] && w[o-1][o-1] = p[z-1][z-1] && w[o+1][o+1] = p[z+1][z+1] && w[o+1][o-1] = p[z+1][z-1] && w[o-1][o+1] = p[z-1][z+1]

let checkWindowXmas (w: string array array) = 
    if (Array.exists (fun x -> x) (Array.map (fun logo -> checkLogo w logo 3 1) logos)) then 1 else 0

let iterateGrid (grid: string array array) (checker: string array array -> int) =
    let windowSize = 7
    let res = [|for r in 0..((Array.length grid - windowSize)) do
                for c in 0..((Array.length grid[0] - windowSize)) do
                    let currentRow = Array.sub grid r windowSize
                    let window = Array.map (fun row -> Array.sub row c windowSize) currentRow
                    checker(window)|]
    Array.sum res 

let padGrid(grid: string array array) (padSize: int) =
    let rows = Array.length grid
    let cols = Array.length grid[0]
    let padded = [|
        for r in 0 .. (rows - 1 + padSize * 2) do
        [|for c in 0 .. (cols - 1 + padSize * 2) do "."|]        
    |]
    for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
            padded[r + padSize][c + padSize] <- grid[r][c]
    padded

let generateGrid(grid: string array) =
    let g = Array.map (fun x -> Array.ofSeq (Seq.map string x)) grid
    iterateGrid (padGrid g 3)

let lines = File.ReadAllLines("input1.txt")
printfn "%d" (generateGrid(lines) checkWindow)
printfn "%d" (generateGrid(lines) checkWindowXmas)