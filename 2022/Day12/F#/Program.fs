open System.IO

let map = File.ReadAllLines("sample.txt")

let getHeight c =
    if c = 'S' then 'a'
    elif c = 'E' then 'z'
    else c

let charAt (x, y) =
    map.[y].[x]

let tryCharAt (x, y) =
    map
    |> Seq.tryItem y
    |> Option.bind (Seq.tryItem x)

let getNextSteps (x, y) visitedPositions =
    let currentHeight = charAt (x, y) |> getHeight
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.filter (fun v -> not <| Set.contains v visitedPositions)
    |> List.choose (fun v ->
        match tryCharAt v |> Option.map getHeight with
        | Some c when (int c = int currentHeight || int c = int currentHeight + 1) -> Some v
        | _ -> None
    )

let rec makeMoves steps visitedPositions currentPosition =
    let currentHeight = charAt currentPosition
    if currentHeight = 'E' then
        printfn "%d" steps
        Some steps
    else
        let visitedPositions' = Set.add currentPosition visitedPositions
        let next = getNextSteps currentPosition visitedPositions'
        // printfn "%A -> %A" currentPosition next
        next
        |> List.map (makeMoves (steps + 1) visitedPositions')
        |> List.choose id
        |> function
        | [] -> None
        | x -> x |> List.min |> Some

makeMoves 0 Set.empty (0, 0)
|> printfn "Part 1: %A"
