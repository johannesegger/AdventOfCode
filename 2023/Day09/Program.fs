open Checked
open System.IO

let parseSequence (text: string) =
    text.Split() |> Seq.map int |> Seq.toList

let getNextValue numbers =
    let rec fn numbers =
        let diffs = numbers |> List.pairwise |> List.map (fun (a, b) -> b - a)
        if diffs |> List.forall ((=) 0) then List.last numbers
        else
            let nextNumber = fn diffs
            List.last numbers + nextNumber
    fn numbers

File.ReadAllLines "input.txt"
|> Seq.map parseSequence
|> Seq.sumBy getNextValue
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.map parseSequence
|> Seq.sumBy (List.rev >> getNextValue)
|> printfn "Part 2: %d"
