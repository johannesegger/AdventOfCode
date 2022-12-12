open System.IO

let map = File.ReadAllLines("input.txt")

let getPositions c =
    map
    |> Seq.indexed
    |> Seq.choose (fun (y, line) ->
        match Seq.tryFindIndex (fun v -> v = c) line with
        | Some x -> Some (x, y)
        | None -> None
    )
let startPosition = getPositions 'S' |> Seq.exactlyOne
let endPosition = getPositions 'E' |> Seq.exactlyOne

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

let getNextSteps (x, y) =
    let currentHeight = charAt (x, y) |> getHeight
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.choose (fun v ->
        match tryCharAt v |> Option.map getHeight with
        | Some c when int c <= int currentHeight + 1 -> Some v
        | _ -> None
    )

let combineKnownPositions a b =
    a
    |> Map.toSeq
    |> Seq.fold (fun knownPositions (k, v) ->
        knownPositions
        |> Map.change k (function
            | Some v' when v' < v -> Some v'
            | _ -> Some v
        )
    ) b

let rec makeMoves steps knownPositions currentPosition =
    match Map.tryFind currentPosition knownPositions with
    | Some v when v <= steps -> knownPositions
    | _ ->
        if charAt currentPosition = 'E' then
            combineKnownPositions (Map.ofList [(currentPosition, steps)]) knownPositions
        else
            let knownPositions' = Map.add currentPosition steps knownPositions
            let next = getNextSteps currentPosition
            next
            |> List.fold (fun knownPositions next ->
                makeMoves (steps + 1) knownPositions next
            ) knownPositions'

makeMoves 0 Map.empty startPosition
|> Map.find endPosition
|> printfn "Part 1: %d"

getPositions 'a'
|> Seq.fold (makeMoves 0) Map.empty
|> Map.find endPosition
|> printfn "Part 2: %d"
