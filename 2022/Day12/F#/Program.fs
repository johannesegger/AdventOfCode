open System.IO

let map = File.ReadAllLines("input.txt")

let getPositions c =
    map
    |> Seq.indexed
    |> Seq.choose (fun (y, row) ->
        match Seq.tryFindIndex (fun v -> v = c) row with
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
    |> List.choose (fun position ->
        match tryCharAt position |> Option.map getHeight with
        | Some c when int c <= int currentHeight + 1 -> Some position
        | _ -> None
    )

let trySetSteps position steps minSteps =
    match Map.tryFind position minSteps with
    | Some v when v <= steps -> None
    | _ -> Map.add position steps minSteps |> Some

let rec makeMoves steps minSteps currentPosition =
    if charAt currentPosition = 'E' then
        trySetSteps currentPosition steps minSteps
        |> Option.defaultValue minSteps
    else
        match trySetSteps currentPosition steps minSteps with
        | None -> minSteps
        | Some minSteps ->
            (minSteps, getNextSteps currentPosition)
            ||> List.fold (makeMoves (steps + 1))

makeMoves 0 Map.empty startPosition
|> Map.find endPosition
|> printfn "Part 1: %d"

getPositions 'a'
|> Seq.fold (makeMoves 0) Map.empty
|> Map.find endPosition
|> printfn "Part 2: %d"
