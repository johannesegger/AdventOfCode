open Checked
open System.IO
open System

let parseEquation (line: string) =
    let parts = line.Split(':', StringSplitOptions.TrimEntries)
    int64 parts.[0], [ for op in parts.[1].Split() -> int op ]

let parse lines =
    lines
    |> Array.map parseEquation

let canSolve operators (result, operands) =
    let rec fn ops acc =
        match ops with
        | [] when acc = result -> true
        | [] -> false
        | _ when acc > result -> false
        | op :: ops ->
            operators
            |> List.exists (fun operator -> fn ops (operator acc op))
    match operands with
    | [] -> result = 0L
    | op :: ops -> fn ops (int64 op)

File.ReadAllLines "input.txt"
|> parse
|> Seq.filter (canSolve [ (fun acc op -> acc * int64 op); (fun acc op -> acc + int64 op) ])
|> Seq.sumBy fst
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parse
|> Seq.filter (canSolve [ (fun acc op -> acc * int64 op); (fun acc op -> acc + int64 op); (fun acc op -> int64 $"%d{acc}%d{op}") ])
|> Seq.sumBy fst
|> printfn "Part 2: %d"
