open Checked
open System.IO

let parse (lines: string[]) =
    lines
    |> Array.map (fun line -> line |> Seq.map (fun v -> int (v - '0')) |> Seq.toArray)

let getStartingPositions (map: int[][]) =
    [
        for row in 0..map.Length - 1 do
        for col in 0..map.[row].Length - 1 do
            if map.[row].[col] = 0 then (row, col)
    ]

let getPaths (map: int[][]) (row, col) =
    let rec fn (row, col) acc =
        let height = map.[row].[col]
        if height = 9 then acc |> Map.change (row, col) (Option.map ((+)1) >> Option.defaultValue 1 >> Some)
        else
            [ (row - 1, col); (row, col + 1); (row + 1, col); (row, col - 1) ]
            |> List.filter (fun (row, col) -> map |> Array.tryItem row |> Option.bind (Array.tryItem col) |> Option.isSome)
            |> List.filter (fun (row, col) -> map.[row].[col] = height + 1)
            |> List.fold (fun acc p -> fn p acc) acc
    fn (row, col) Map.empty

let getAllPaths map =
    getStartingPositions map
    |> List.map (getPaths map)

File.ReadAllLines "input.txt"
|> parse
|> getAllPaths
|> List.sumBy Map.count
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parse
|> getAllPaths
|> Seq.sumBy (Map.values >> Seq.sum)
|> printfn "Part 2: %d"
