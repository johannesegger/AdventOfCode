open Checked
open System.IO
open System

let parseLine (line: string) =
    let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    (int parts.[0], int parts[1])

let parseLines lines =
    let pairs = lines |> Array.map parseLine
    let listA = pairs |> Array.map fst |> Array.sort
    let listB = pairs |> Array.map snd |> Array.sort
    (listA, listB)

let getDistanceSum (listA, listB) =
    Array.zip listA listB
    |> Array.sumBy (fun (a, b) -> Int32.Abs(a - b))

File.ReadAllLines "input.txt"
|> parseLines
|> getDistanceSum
|> printfn "Part 1: %d"

let getSimilarityScore (listA, listB) =
    listA
    |> Array.sumBy (fun a ->
        listB |> Array.sumBy (fun b -> if a = b then b else 0)
    )

File.ReadAllLines "input.txt"
|> parseLines
|> getSimilarityScore
|> printfn "Part 2: %d"
