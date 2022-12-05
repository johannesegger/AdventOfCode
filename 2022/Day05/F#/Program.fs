open System
open System.IO
open System.Text.RegularExpressions

let parseStacks (lines: string array) =
    let stackWidth = 4
    Seq.initInfinite (fun i -> i * stackWidth + 1)
    |> Seq.takeWhile (fun i -> i < lines.[0].Length)
    |> Seq.map (fun i ->
        lines
        |> Seq.map (fun line -> line.[i])
        |> Seq.filter Char.IsLetter
        |> Seq.toList
    )
    |> Seq.toList

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
    |> Array.toList

let executeMove9000 (stacks: char list list) (count, source, target) =
    let sourceStack = stacks.[source - 1]
    let newSourceStack = sourceStack |> List.skip count
    let newTargetStack =
        let sourceCrates = sourceStack |> List.take count
        List.rev sourceCrates @ stacks.[target - 1]
    stacks
    |> List.updateAt (source - 1) newSourceStack
    |> List.updateAt (target - 1) newTargetStack

let getTopCrates (stacks: char list list) =
    stacks
    |> List.map List.head
    |> List.toArray
    |> String

(stacks, moves)
||> List.fold executeMove9000
|> getTopCrates
|> printfn "Part 1: %s"

let executeMove9001 (stacks: char list list) (count, source, target) =
    stacks
    |> List.updateAt (target - 1) (List.take count stacks.[source - 1] @ stacks.[target - 1])
    |> List.updateAt (source - 1) (List.skip count stacks.[source - 1])

(stacks, moves)
||> List.fold executeMove9001
|> getTopCrates
|> printfn "Part 2: %s"
