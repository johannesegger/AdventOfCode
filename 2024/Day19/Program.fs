open Checked
open System
open System.IO

let parse (lines: string[]) =
    let towels = lines.[0].Split(',', StringSplitOptions.TrimEntries)
    let patterns = lines.[2..]
    (towels, patterns)

let getCombinationCount (towels: string array) pattern =
    let rec fn patterns =
        match patterns with
        | [] -> 0L
        | [ ("", v) ] -> v
        | patterns ->
            let patterns' =
                [
                    for (pattern, count) in patterns do
                        if pattern = "" then (pattern, count)
                        for towel in towels do
                            if pattern.StartsWith towel then
                                (pattern.[towel.Length ..], count)
                ]
                |> List.groupBy fst
                |> List.map (fun (pattern, counts) -> (pattern, counts |> Seq.sumBy snd))
            fn patterns'

    fn [(pattern, 1L)]

let getPossiblePatterns (towels, patterns) =
    patterns
    |> Seq.map (getCombinationCount towels)

File.ReadAllLines "input.txt"
|> parse
|> getPossiblePatterns
|> Seq.filter (fun v -> v > 0)
|> Seq.length
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parse
|> getPossiblePatterns
|> Seq.sum
|> printfn "Part 2: %d"
