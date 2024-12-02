open Checked
open System.IO

let parseLine (line: string) =
    line.Split(' ') |> Array.map int |> Array.toList

let parse lines =
    lines
    |> Array.map parseLine
    |> Array.toList

let isSafe line =
    (List.sort line = line || List.sortDescending line = line) &&
    line |> List.pairwise |> List.forall (fun (a, b) -> let diff = abs (a - b) in diff >= 1 && diff <= 3)

File.ReadAllLines "input.txt"
|> parse
|> List.filter isSafe
|> List.length
|> printfn "Part 1: %d"

let isSafe' line =
    [ for i in 0..List.length line - 1 -> List.removeAt i line]
    |> List.exists isSafe

File.ReadAllLines "input.txt"
|> parse
|> List.filter isSafe'
|> List.length
|> printfn "Part 2: %d"
