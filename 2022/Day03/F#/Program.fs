open System.IO
open System.Diagnostics

let getPrio c =
    Debug.Assert((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

    if c >= 'a' && c <= 'z' then (int c) - (int 'a') + 1
    else (int c) - (int 'A') + 27

let lines = File.ReadAllLines("input.txt")

lines
|> Seq.map (fun line ->
    let compartmentA = line.Substring(0, line.Length / 2) |> Set.ofSeq
    let compartmentB = line.Substring(line.Length / 2) |> Set.ofSeq
    Set.intersect compartmentA compartmentB |> Seq.exactlyOne
    |> getPrio
)
|> Seq.sum
|> printfn "Part 1: %d"

lines
|> Seq.chunkBySize 3
|> Seq.map (fun chunk ->
    chunk
    |> Seq.map Set.ofSeq
    |> Seq.reduce Set.intersect
    |> Seq.exactlyOne
    |> getPrio
)
|> Seq.sum
|> printfn "Part 2: %d"
