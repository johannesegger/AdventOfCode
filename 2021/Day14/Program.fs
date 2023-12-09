open Checked
open System.IO

let parseInsertionRule (line: string) =
    let parts = line.Split(" -> ")
    ((parts.[0].[0], parts.[0].[1]), parts.[1].[0])

let parseFormula (lines: string array) =
    let template = lines.[0]

    let insertionRules =
        lines
        |> Seq.skip 2
        |> Seq.map parseInsertionRule
        |> Map.ofSeq

    (template, insertionRules)

let run steps (template: string, insertionRules) =
    let rec fn steps template =
        if steps = 0 then template
        else
            let template' =
                (Map.empty, template)
                ||> Map.fold (fun template (a, b) ab ->
                    let c = Map.find (a, b) insertionRules
                    let ac = Map.tryFind (a, c) template |> Option.defaultValue 0L
                    let cb = Map.tryFind (c, b) template |> Option.defaultValue 0L
                    template
                    |> Map.add (a, c) (ac + ab)
                    |> Map.add (c, b) (cb + ab)
                )
            fn (steps - 1) template'
    let templateMap =
        template
        |> Seq.pairwise
        |> Seq.groupBy id
        |> Seq.map (fun (group, list) -> (group, Seq.length list |> int64))
        |> Map.ofSeq
    let quantities =
        fn steps templateMap
        |> Map.toSeq
        |> Seq.map (fun ((a, b), count) -> (b, count))
        |> Seq.append [ (template.[0], 1) ]
        |> Seq.groupBy fst
        |> Seq.map (fun (c, counts) -> counts |> Seq.sumBy snd)
    Seq.max quantities - Seq.min quantities

File.ReadAllLines "input.txt"
|> parseFormula
|> run 10
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parseFormula
|> run 40
|> printfn "Part 2: %d"
