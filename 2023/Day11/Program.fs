open Checked
open System.IO

let parseImage (lines: string array) =
    [
        for row in 0..lines.Length - 1 do
        for col in 0..lines.[row].Length - 1 do
        if lines.[row].[col] = '#' then (int64 row, int64 col)
    ]

let expandDirection getCoordinate offsetCoordinate factor image =
    let maxCoord = image |> List.map getCoordinate |> List.max
    ((0L, []), [0L..maxCoord])
    ||> List.fold (fun (offset, expandedImage) coord ->
        let galaxiesAtCoord = image |> List.filter (getCoordinate >> (=) coord)
        if List.isEmpty galaxiesAtCoord then (offset - 1L + factor, expandedImage)
        else (offset, galaxiesAtCoord |> List.map (fun position -> offsetCoordinate position offset) |> List.append expandedImage)
    )
    |> snd

let expandRows = expandDirection fst (fun (row, col) offset -> (row + offset, col))
let expandColumns = expandDirection snd (fun (row, col) offset -> (row, col + offset))
let expandEmptySpace factor = expandRows factor >> expandColumns factor

let getPairs list =
    let rec fn list pairs =
        match list with
        | [] -> pairs
        | a :: rest ->
            let pairs' = [ for b in rest -> (a, b) ] @ pairs
            fn rest pairs'
    fn list []

let getShortestPath ((row1, col1), (row2, col2)) =
    abs (row2 - row1) + abs (col2 - col1)

let getSumOfShortestPaths galaxies =
    getPairs galaxies
    |> List.sumBy getShortestPath

File.ReadAllLines "input.txt"
|> parseImage
|> expandEmptySpace 2L
|> getSumOfShortestPaths
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parseImage
|> expandEmptySpace 1_000_000L
|> getSumOfShortestPaths
|> printfn "Part 2: %d"
