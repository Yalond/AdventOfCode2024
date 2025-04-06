open System.IO
open System.Text.RegularExpressions

let clean (x:string) =  
    let g = (x.[4..(x.Length - 2)]).Split [|','|]
    (int g[0]) * (int g[1])

let rec matchTokens xs shouldAdd =
    match xs with
    | [] -> 0
    | head :: _ -> 
        match head with
        | "don't()" -> matchTokens (List.tail xs) 0
        | "do()" -> matchTokens (List.tail xs) 1
        | _ -> clean(head) * shouldAdd + matchTokens (List.tail xs) shouldAdd

let p path pattern =

    let matches = Regex.Matches((String.concat " " (File.ReadAllLines(path))), pattern)
    printfn "%d" ((matches |> Seq.map (fun m -> m.Value) |> Seq.toList |> matchTokens) 1)

let regexes = [@"mul\(\d*\,\d*\)"; @"mul\(\d*\,\d*\)|don\'t\(\)|do\(\)"]
regexes |> List.map (p "input1.txt") 