open Checked
open System.IO
open System.Text.RegularExpressions

let parse (content: string) =
    Regex.Matches(content, @"mul\((\d+),(\d+)\)")
    |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups[2].Value))
    |> Seq.toList

File.ReadAllText "input.txt"
|> parse
|> Seq.sumBy (fun (a, b) -> a * b)
|> printfn "Part 1: %A"

let parse' (content: string) =
    Regex.Matches(content, @"mul\((\d+),(\d+)\)|don't\(\)|do\(\)")
    |> Seq.fold
        (fun (l, isOn) m ->
            if m.Value.StartsWith("mul") then
                if isOn then l @ [ (int m.Groups.[1].Value, int m.Groups[2].Value) ], isOn
                else l, isOn
            elif m.Value = "do()" then l, true
            elif m.Value = "don't()" then l, false
            else failwith $"Unexpected match: %s{m.Value}"
        )
        ([], true)
    |> fst
    |> Seq.toList

File.ReadAllText "input.txt"
|> parse'
|> Seq.sumBy (fun (a, b) -> a * b)
|> printfn "Part 2: %A"
