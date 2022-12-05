open System
open System.IO
open System.Text.RegularExpressions

module String =
    let rev v = v |> Seq.rev |> Seq.toArray |> String

type Stack = Stack of string
module Stack =
    let pop count (Stack v) =
        Stack (v.Substring(0, count)),
        Stack (v.Substring(count))
    let push (Stack source) (Stack target) =
        String.rev source + target
        |> Stack
    let pushAll (Stack source) (Stack target) =
        source + target
        |> Stack
    let peek (Stack v) = v.[0]

let parseStacks (lines: string array) =
    lines
    |> Seq.transpose
    |> Seq.map (Seq.toArray >> String)
    |> Seq.filter (fun v -> Char.IsDigit v.[v.Length - 1])
    |> Seq.map (fun v -> v.Substring(0, v.Length - 1).TrimStart() |> Stack)
    |> Seq.toArray

let lines = File.ReadAllLines("input.txt")

let stacks =
    lines
    |> Array.takeWhile (not << String.IsNullOrEmpty)
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

let executeMove9000 (stacks: Stack array) (count, source, target) =
    let (popped, newSourceStack) = Stack.pop count stacks.[source - 1]
    let newTargetStack = Stack.push popped stacks.[target - 1]
    stacks
    |> Array.updateAt (source - 1) newSourceStack
    |> Array.updateAt (target - 1) newTargetStack

let getTopCrates stacks =
    stacks
    |> Array.map Stack.peek
    |> String

(stacks, moves)
||> List.fold executeMove9000
|> getTopCrates
|> printfn "Part 1: %s"

let executeMove9001 (stacks: Stack array) (count, source, target) =
    let (popped, newSourceStack) = Stack.pop count stacks.[source - 1]
    let newTargetStack = Stack.pushAll popped stacks.[target - 1]
    stacks
    |> Array.updateAt (source - 1) newSourceStack
    |> Array.updateAt (target - 1) newTargetStack

(stacks, moves)
||> List.fold executeMove9001
|> getTopCrates
|> printfn "Part 2: %s"
