open Checked
open System.IO

type BlockItem = MirrorSlash | MirrorBackslash | VerticalSplitter | HorizontalSplitter

let tryParseBlock = function
    | '/' -> Some MirrorSlash
    | '\\' -> Some MirrorBackslash
    | '|' -> Some VerticalSplitter
    | '-' -> Some HorizontalSplitter
    | c -> None

let parseMap (lines: string array) =
    [
        for row in 0..lines.Length - 1 do
        for col in 0..lines.[row].Length - 1 do
            match tryParseBlock lines.[row].[col] with
            | Some v -> ((row, col), v)
            | None -> ()
    ]
    |> Map.ofList

type Direction = Up | Down | Left | Right
let moveInDirection (row, col) = function
    | Up -> (row - 1, col)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)

let moveBeam map ((row, col), direction) =
    let maxRow = map |> Map.keys |> Seq.map fst |> Seq.max
    let maxCol = map |> Map.keys |> Seq.map snd |> Seq.max
    let beams =
        match map |> Map.tryFind (row, col), direction with
        | None, _ -> [ ((row, col), direction) ]
        | Some MirrorSlash, Up -> [ ((row, col), Right) ]
        | Some MirrorSlash, Down -> [ ((row, col), Left) ]
        | Some MirrorSlash, Left -> [ ((row, col), Down) ]
        | Some MirrorSlash, Right -> [ ((row, col), Up) ]
        | Some MirrorBackslash, Up -> [ ((row, col), Left) ]
        | Some MirrorBackslash, Down -> [ ((row, col), Right) ]
        | Some MirrorBackslash, Left -> [ ((row, col), Up) ]
        | Some MirrorBackslash, Right -> [ ((row, col), Down) ]
        | Some VerticalSplitter, Up
        | Some VerticalSplitter, Down -> [ ((row, col), direction) ]
        | Some VerticalSplitter, Left
        | Some VerticalSplitter, Right -> [ ((row, col), Up); ((row, col), Down) ]
        | Some HorizontalSplitter, Left
        | Some HorizontalSplitter, Right -> [ ((row, col), direction) ]
        | Some HorizontalSplitter, Up
        | Some HorizontalSplitter, Down -> [ ((row, col), Left); ((row, col), Right) ]
    beams
    |> List.map (fun (position, direction) -> (moveInDirection position direction, direction))
    |> List.filter (fun ((row, col), direction) -> row >= 0 && row <= maxRow && col >= 0 && col <= maxCol)

let getNumberOfEnergizedTiles startPosition map =
    let rec fn beams visitedPositions =
        if Set.difference beams visitedPositions = Set.empty then
            visitedPositions |> Set.map fst |> Set.count
        else
            let visitedPositions' = Set.union visitedPositions beams
            let beams' = beams |> Seq.collect (moveBeam map) |> Set.ofSeq |> fun v -> Set.difference v visitedPositions
            fn beams' visitedPositions'
    fn (Set.ofList [ startPosition ]) Set.empty

File.ReadAllLines "input.txt"
|> parseMap
|> getNumberOfEnergizedTiles ((0, 0), Right)
|> printfn "Part 1: %d"

let getNumberOfEnergizedTilesOfBestStartPosition map =
    let maxRow = map |> Map.keys |> Seq.map fst |> Seq.max
    let maxCol = map |> Map.keys |> Seq.map snd |> Seq.max
    let startPositions = [
        for row in 0..maxRow do
            ((row, 0), Right)
            ((row, maxCol), Left)
        for col in 0..maxCol do
            ((0, col), Down)
            ((maxRow, col), Up)
    ]
    startPositions
    |> List.map (fun startPosition ->
        getNumberOfEnergizedTiles startPosition map
    )
    |> List.max

File.ReadAllLines "input.txt"
|> parseMap
|> getNumberOfEnergizedTilesOfBestStartPosition
|> printfn "Part 2: %d"
