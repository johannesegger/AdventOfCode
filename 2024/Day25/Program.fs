open Checked
open System.IO

let parse (lines: string[]) =
    lines
    |> Seq.chunkBySize 8
    |> Seq.map (Array.take 7)
    |> Seq.toList
    |> Seq.fold (fun (locks, keys) v -> if v.[0].[0] = '#' then (v :: locks, keys) else (locks, v :: keys)) ([], [])

let getLockHeights lock =
    lock |> Seq.transpose |> Seq.map (fun v -> (Seq.takeWhile ((=) '#') v |> Seq.length) - 1)

let canCombine (lock, key) =
    Seq.zip (getLockHeights lock) (getLockHeights (Seq.rev key))
    |> Seq.forall (fun (a, b) -> a + b <= 5)

let getPossibleCombinations (locks, keys) =
    Seq.allPairs locks keys
    |> Seq.filter canCombine

File.ReadAllLines "input.txt"
|> parse
|> getPossibleCombinations
|> Seq.length
|> printfn "Part 1: %d"
