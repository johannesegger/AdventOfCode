let parseLine (line: string) =
    line.Split '/'
    |> Array.map int
    |> function
    | [| a; b |] -> a, b
    | _ -> failwithf "Can't parse line: '%s'" line

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day24-input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let rec build bridges =
    bridges
    |> Seq.collect (fun (bridge, availableComponents) ->
        let validPort =
            match bridge with
            | [] -> 0
            | (_, b) :: _ -> b
        let validComponents =
            availableComponents
            |> List.choose (fun (idx, (a', b')) ->
                if validPort = a' then Some (idx, (a', b'))
                elif validPort = b' then Some (idx, (b', a'))
                else None
            )
            |> List.distinctBy snd
        match validComponents with
        | [] -> Seq.singleton bridge
        | v -> 
            v
            |> Seq.map (fun (idx, c) ->
                c :: bridge, availableComponents |> List.filter (fst >> ((<>)idx))
            )
            |> build
    )

let getBridgeStrength = List.sumBy (fun (a, b) -> a + b)

let solution1 =
    build [ [], List.indexed input ]
    |> Seq.map getBridgeStrength
    |> Seq.max

let solution2 =
    build [ [], List.indexed input ]
    |> Seq.map (fun bridge -> List.length bridge, getBridgeStrength bridge)
    |> Seq.max
    |> snd
