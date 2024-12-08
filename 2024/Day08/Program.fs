open Checked
open System.IO

let calculateAntinode (row, col) (r, c) =
    (row - (r - row), col - (c - col))

let isOnMap map (row, col) =
    map |> Seq.tryItem row |> Option.bind (Seq.tryItem col) |> Option.isSome

let antennaPositions (map: string[]) =
    [
        for row in 0..map.Length - 1 do
        for col in 0..map.[row].Length - 1 do
            if map.[row].[col] <> '.' then (row, col)
    ]

let coAntennaPositions (map: string[]) (row, col) =
    let antenna = map.[row].[col]
    antennaPositions map
    |> List.filter ((<>) (row, col))
    |> List.filter (fun (r, c) -> map.[r].[c] = antenna)

let getAntinodesOfAntenna (map: string array) (row, col) =
    coAntennaPositions map (row, col)
    |> List.map (calculateAntinode (row, col))
    |> List.filter (isOnMap map)

let getAntinodes map =
    antennaPositions map
    |> List.collect (getAntinodesOfAntenna map) 
    |> List.distinct

File.ReadAllLines "input.txt"
|> getAntinodes
|> List.length
|> printfn "Part 1: %d"

let calculateAntinodes map (row, col) (r, c) =
    let (dCol, dRow) = (c - col, r - row)
    [
        yield!
            Seq.initInfinite (fun i -> (row - dRow * i, col - dCol * i))
            |> Seq.takeWhile (isOnMap map)
        yield!
            Seq.initInfinite (fun i -> (row + dRow * i, col + dCol * i))
            |> Seq.takeWhile (isOnMap map)
    ]

let getAntinodesOfAntenna' (map: string array) (row, col) =
    coAntennaPositions map (row, col)
    |> List.collect (calculateAntinodes map (row, col))

let getAntinodes' map =
    antennaPositions map
    |> List.collect (getAntinodesOfAntenna' map) 
    |> List.distinct

File.ReadAllLines "input.txt"
|> getAntinodes'
|> List.length
|> printfn "Part 2: %d"
