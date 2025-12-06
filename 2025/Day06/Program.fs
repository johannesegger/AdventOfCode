open Checked
open System
open System.IO

let parseOperator v =
    if v = "*" then (fun a b -> a * b)
    elif v = "+" then (fun a b -> a + b)
    else failwith $"Unknown operator: %s{v}"

let parse (lines: string[]) =
    let parts =
        lines
        |> Seq.map (fun v -> v.Split(' ', StringSplitOptions.RemoveEmptyEntries))
        |> Seq.toList
    let numbers =
        parts
        |> List.take (parts.Length - 1)
        |> List.map (fun v -> v |> Seq.map int64 |> Seq.toList)
        |> List.transpose
    let operators =
        parts
        |> List.last
        |> Seq.map parseOperator
        |> Seq.toList
    List.zip numbers operators

let calculateResult (numbers, fn) =
    List.reduce fn numbers

File.ReadAllLines "input.txt"
|> parse
|> List.sumBy calculateResult
|> printfn "Part 1: %d"

module List =
    let splitBy fn (list: 'a list) =
        let rec f list currentChunk acc =
            match list with
            | x :: xs -> 
                if fn x then f xs [] (List.rev currentChunk :: acc)
                else f xs (x :: currentChunk) acc
            | [] -> currentChunk :: acc |> List.rev
        f list [] []

let parseCols (lines: string[]) =
    let numbers =
        lines
        |> Seq.take (lines.Length - 1)
        |> Seq.transpose
        |> Seq.toList
        |> List.splitBy (Seq.forall ((=) ' '))
        |> List.map (fun cols ->
            cols
            |> List.map (Seq.toArray >> String >> int64)
        )
    let operators =
        lines
        |> Seq.last
        |> fun v -> v.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseOperator
        |> Seq.toList
    List.zip numbers operators

File.ReadAllLines "input.txt"
|> parseCols
|> List.sumBy calculateResult
|> printfn "Part 2: %d"
