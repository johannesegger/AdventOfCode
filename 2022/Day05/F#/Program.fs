open System
open System.IO
open System.Text.RegularExpressions

let parseStacks (lines: string array) =
    let stackWidth = 4
    Array.init ((lines.[0].Length + 1) / stackWidth) (fun stackIndex ->
        lines
        |> Array.map (fun line ->
            let column = stackIndex * 4 + 1
            line.[column]
        )
        |> Array.filter (fun c -> c <> ' ')
        |> Array.toList
    )

let lines = File.ReadAllLines("input.txt")
let stacks =
    lines
    |> Array.takeWhile (not << String.IsNullOrEmpty)
    |> fun v -> Array.take (v.Length - 1) v
    |> parseStacks

let parseMove line =
    let m = Regex.Match(line, @"^move (\d+) from (\d+) to (\d+)$")
    let count = int m.Groups.[1].Value
    let source = int m.Groups[2].Value
    let target = int m.Groups[3].Value
    (count, source, target)

let moves =
    lines
    |> Array.skipWhile (not << String.IsNullOrEmpty)
    |> Array.skip 1
    |> Array.map parseMove

let rec executeMove9000 (stacks: char list array) (count, source, target) =
    match count, stacks.[source - 1] with
    | 0, _ -> stacks
    | count, x :: xs ->
        stacks.[source - 1] <- xs
        stacks.[target - 1] <- x :: stacks.[target - 1]
        executeMove9000 stacks (count - 1, source, target)
    | _, [] -> failwith "Can't execute move: empty stack"

let getStackTop (stacks: char list array) =
    stacks
    |> Array.map (function
        | x :: xs -> x
        | [] -> failwith "Can't get stack top: empty stack"
    )
    |> fun v -> String(v)

// (stacks, moves)
// ||> Array.fold executeMove9000
// |> getStackTop
// |> printfn "Part 1: %s"

let rec executeMove9001 (stacks: char list array) (count, source, target) =
    stacks.[target - 1] <- (stacks.[source - 1] |> List.take count) @ stacks.[target - 1]
    stacks.[source - 1] <- stacks.[source - 1] |> List.skip count
    stacks

(stacks, moves)
||> Array.fold executeMove9001
|> getStackTop
|> printfn "Part 2: %s"
