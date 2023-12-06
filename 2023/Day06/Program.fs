open Checked
open System
open System.IO

let parseRaces (lines: string array) =
    let times = lines.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> Seq.map int64
    let distances = lines.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> Seq.map int64
    Seq.zip times distances |> Seq.toList

let getNumberOfWaysToWin (time, minDistance) =
    [0L..time]
    |> List.filter (fun speed -> speed * (time - speed) > minDistance)
    |> List.length

File.ReadAllLines "input.txt"
|> parseRaces
|> List.map getNumberOfWaysToWin
|> Seq.reduce (*)
|> printfn "Part 1: %d"

let parseSingleRace (lines: string array) =
    let time = lines.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> String.concat ""
    let distance = lines.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> String.concat ""
    (int64 time, int64 distance)

File.ReadAllLines "input.txt"
|> parseSingleRace
|> getNumberOfWaysToWin
|> printfn "Part 2: %d"
