open FSharp.Core.Operators.Checked
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics

type Monkey = {
    Items: int64 list
    Operation: int64 -> int64
    DivisibleBy: int
    IfTrue: int
    IfFalse: int
    TotalItemCount: int64
}

let parseMonkey (lines: string array) =
    {
        Items =
            Regex.Match(lines.[1], "Starting items: (.*)$").Groups.[1].Value.Split(", ")
            |> Seq.map int64
            |> Seq.toList
        Operation =
            let m = Regex.Match(lines.[2], @"Operation: new = old (\*|\+) (.+)$")
            match m.Groups.[1].Value, m.Groups.[2].Value with
            | "*", "old" -> (fun x -> x * x)
            | "+", "old" -> (fun x -> x + x)
            | "*", v -> let v = int64 v in (fun x -> x * v)
            | "+", v -> let v = int64 v in (fun x -> x + v)
            | a, b -> failwith $"Invalid operator or operand: %A{(a, b)}"
        DivisibleBy = Regex.Match(lines.[3], @"Test: divisible by (\d+)$").Groups[1].Value |> int
        IfTrue = Regex.Match(lines.[4], @"If true: throw to monkey (\d+)$").Groups.[1].Value |> int
        IfFalse = Regex.Match(lines.[5], @"If false: throw to monkey (\d+)$").Groups.[1].Value |> int
        TotalItemCount = 0L
    }

let throwItem mapWorryLevel monkeyIndex (monkeys: Monkey array) item =
    let monkey = monkeys.[monkeyIndex]
    let worryLevel = monkey.Operation item |> mapWorryLevel
    let next = if worryLevel % (int64 monkey.DivisibleBy) = 0L then monkey.IfTrue else monkey.IfFalse
    monkeys
    |> Array.mapi (fun i v ->
        if i = monkeyIndex then
            Debug.Assert(List.head v.Items = item, "Expected head item to be removed")
            { v with Items = List.tail v.Items; TotalItemCount = v.TotalItemCount + 1L }
        elif i = next then { v with Items = v.Items @ [ worryLevel ] }
        else v
    )

let throwItems mapWorryLevel (monkeys: Monkey array) monkeyIndex =
    (monkeys, monkeys.[monkeyIndex].Items)
    ||> List.fold (throwItem mapWorryLevel monkeyIndex)

let playRound mapWorryLevel (monkeys: Monkey array) =
    (monkeys, [0..monkeys.Length - 1])
    ||> List.fold (throwItems mapWorryLevel)

let rec playRounds (count: int) mapWorryLevel monkeys =
    if count = 0 then
        monkeys
        |> Seq.map (fun v -> v.TotalItemCount)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.reduce (fun a b -> a * b)
    else
        let monkeys' = playRound mapWorryLevel monkeys
        playRounds (count - 1) mapWorryLevel monkeys'

let monkeys =
    File.ReadAllLines("input.txt")
    |> Seq.chunkBySize 7
    |> Seq.map parseMonkey
    |> Seq.toArray

monkeys
|> playRounds 20 (fun worryLevel -> worryLevel / 3L)
|> printfn "Part 1: %d"

let divider =
    monkeys
    |> Seq.map (fun v -> int64 v.DivisibleBy)
    |> Seq.reduce (fun a b -> a * b)

monkeys
|> playRounds 10_000 (fun worryLevel -> worryLevel % divider)
|> printfn "Part 2: %d"
