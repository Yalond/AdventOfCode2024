let printList (xs: 'a array) = printfn "%s" (String.concat " " (Array.map (fun x -> x.ToString()) xs))
let mapFromPairs () = 
    Array.fold (fun acc (key, value) -> 
        acc |> Map.change key (function
            | Some values -> Some (value :: values)
            | None -> Some [value]
            )
        ) Map.empty

