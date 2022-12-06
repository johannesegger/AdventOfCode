open System.IO

let input = File.ReadAllText("input.txt")

let findMarker numberOfDistinctCharacters =
    input
    |> Seq.windowed numberOfDistinctCharacters
    |> Seq.findIndex (fun v -> v |> Set.ofSeq |> Set.count = numberOfDistinctCharacters)
    |> fun i -> i + numberOfDistinctCharacters

findMarker 4
|> printfn "Part 1: %d"

findMarker 14
|> printfn "Part 2: %d"
