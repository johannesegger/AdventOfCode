open Checked
open System.IO

let getCoords row col =
    [
        [ for i in [0..3] -> (row - i, col) ] // up
        [ for i in [0..3] -> (row - i, col + i) ] // up right
        [ for i in [0..3] -> (row, col + i) ] // right
        [ for i in [0..3] -> (row + i, col + i) ] // down right
        [ for i in [0..3] -> (row + i, col) ] // down
        [ for i in [0..3] -> (row + i, col - i) ] // down left
        [ for i in [0..3] -> (row, col - i) ] // left
        [ for i in [0..3] -> (row - i, col - i) ] // up left
    ]

let getTexts (lines: string array) coords =
    coords
    |> Seq.filter (Seq.forall (fun (row, col) -> lines |> Array.tryItem row |> Option.bind (Seq.tryItem col) |> Option.isSome))
    |> Seq.map (Seq.map (fun (row, col) -> string lines.[row].[col]) >> String.concat "")

let getXMASLocations (lines: string array) =
    [ for row in [0..lines.Length - 1] do
        for col in [0..lines.[row].Length - 1] do
            for text in getCoords row col |> getTexts lines do
                if text = "XMAS" then (row, col)
    ]


File.ReadAllLines "input.txt"
|> getXMASLocations
|> Seq.length
|> printfn "Part 1: %d"

let getDiagonalCoords row col =
    [
        [ for i in [-1..1] -> (row + i, col + i) ] // left to right
        [ for i in [-1..1] -> (row - i, col + i) ] // right to left
    ]

let getXMASLocations' (lines: string array) =
    [ for row in [0..lines.Length - 1] do
        for col in [0..lines.[row].Length - 1] do
            let texts = getDiagonalCoords row col |> getTexts lines |> Seq.toList
            if texts.Length = 2 && texts |> List.forall (fun text -> text = "MAS" || text = "SAM") then (row, col)
    ]

File.ReadAllLines "input.txt"
|> getXMASLocations'
|> Seq.length
|> printfn "Part 2: %d"
