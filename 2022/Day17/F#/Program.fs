open FSharp.Core.Operators.Checked
open System.IO

let parseDirection = function
    | '<' -> -1
    | '>' -> 1
    | c -> failwith $"Invalid direction: \"%c{c}\""

let stream =
    File.ReadAllText("input.txt")
    |> Seq.map parseDirection
    |> Seq.toArray

let parseRock (v: string) =
    let lines = v.Split("\r\n")
    let maxRow = Array.length lines - 1
    let height = maxRow + 1
    let width = String.length lines.[0]
    let coordinates =
        [
            for row in 0..maxRow do
            for column in 0..String.length lines.[row] - 1 do
                if lines.[row].[column] = '#' then (column, maxRow - row)
        ]
    (width, height, coordinates)

let rocks =
    File.ReadAllText("rocks.txt").Split("\r\n\r\n")
    |> Array.map parseRock

let chamberWidth = 7

let isRockPositionBlocked map (rockX, rockY) ((rockPartX, rockPartY)) =
    let (totalRockPartX, totalRockPartY) = (rockX + rockPartX, rockY + rockPartY)
    totalRockPartX < 0 || totalRockPartX >= chamberWidth ||
    totalRockPartY < 0 ||
    Set.contains (totalRockPartX, totalRockPartY) map

let tryMoveRock map rockCoordinates (deltaX, deltaY) (rockX, rockY) =
    let delta = (rockX + deltaX, rockY + deltaY)
    let cannotMove =
        rockCoordinates
        |> List.exists (isRockPositionBlocked map delta)
    if cannotMove then None
    else Some delta

let addRockToMap rock (deltaX, deltaY) map =
    (map, rock)
    ||> List.fold (fun map (rockX, rockY) ->
        Set.add (rockX + deltaX, rockY + deltaY) map
    )

let rec simulateRock streamIndex (rockWidth, rockHeight, rockCoordinates) rockPosition map =
    match tryMoveRock map rockCoordinates (0, -1) rockPosition with
    | Some p ->
        let rockPosition =
            match tryMoveRock map rockCoordinates (stream.[streamIndex], 0) p with
            | Some p -> p
            | None -> p
        simulateRock ((streamIndex + 1) % stream.Length) (rockWidth, rockHeight, rockCoordinates) rockPosition map
    | None ->
        let totalHeight = snd rockPosition + rockHeight
        let map' = addRockToMap rockCoordinates rockPosition map
        (streamIndex, totalHeight, map')

let nextStreamItems count startIndex =
    List.init count (fun i -> (startIndex + i) % stream.Length)
    |> List.map (fun i -> stream.[i])

let getRockStartX streamIndex rockWidth =
    let x =
        (2, nextStreamItems 4 streamIndex)
        ||> List.fold (fun x dx -> x + dx |> max 0 |> min (chamberWidth - rockWidth))
    (streamIndex + 4) % stream.Length, x

type State = {
    StreamIndex: int
    RockIndex: int
    TotalHeight: int
    BlockMap: Set<int * int>
}
module State =
    let init =
        {
            StreamIndex = 0
            RockIndex = 0
            TotalHeight = 0
            BlockMap = Set.empty
        }

let run state =
    let (rockWidth, rockHeight, rockCoordinates) = rocks.[state.RockIndex]
    let (streamIndex', rockStartX) = getRockStartX state.StreamIndex rockWidth
    let rockPosition = (rockStartX, state.TotalHeight)
    let (streamIndex'', totalHeight', map') = simulateRock streamIndex' (rockWidth, rockHeight, rockCoordinates) rockPosition state.BlockMap
    {
        StreamIndex = streamIndex''
        RockIndex = (state.RockIndex + 1) % rocks.Length
        TotalHeight = max state.TotalHeight totalHeight'
        BlockMap = map'
    }

let states initialState =
    initialState
    |> Seq.unfold (fun state ->
        let nextState = run state
        Some(nextState, nextState)
    )
    |> Seq.append [ initialState ]

states State.init
|> Seq.item 2022
|> fun state -> printfn "Part 1: %d" state.TotalHeight

let (repetitionStart, repetitionEnd, repetitionStartStreamIndex, repetitionStartRockIndex) =
    ((Map.empty, None), states State.init |> Seq.indexed)
    ||> Seq.scan (fun (repetitionMap, _) (totalNumberOfRocks, state) ->
        match Map.tryFind (state.StreamIndex, state.RockIndex) repetitionMap with
        | Some (second :: first :: _) when totalNumberOfRocks - second = second - first ->
            (repetitionMap, Some (first, second, state.StreamIndex, state.RockIndex))
        | Some l ->
            let map' = Map.add (state.StreamIndex, state.RockIndex) (totalNumberOfRocks :: l) repetitionMap
            (map', None)
        | None ->
            let map' = Map.add (state.StreamIndex, state.RockIndex) [ totalNumberOfRocks ] repetitionMap
            (map', None)
    )
    |> Seq.choose snd
    |> Seq.head

let totalHeight =
    let repetitionLength = repetitionEnd - repetitionStart
    let totalIterations = 1_000_000_000_000L
    let repeatedIterations = (totalIterations - int64 repetitionStart) / int64 repetitionLength

    let stateBeforeRepetition =
        states State.init |> Seq.item repetitionStart
    let stateAfterRepetition =
        stateBeforeRepetition
        |> states
        |> Seq.item repetitionLength
    let stateAtEnd =
        let count = totalIterations - int64 repetitionStart - repeatedIterations * int64 repetitionLength
        stateAfterRepetition
        |> states
        |> Seq.item (int count)
    int64 stateBeforeRepetition.TotalHeight +
    repeatedIterations * int64 (stateAfterRepetition.TotalHeight - stateBeforeRepetition.TotalHeight) +
    int64 (stateAtEnd.TotalHeight - stateAfterRepetition.TotalHeight)
printfn "Part 2: %d" totalHeight
