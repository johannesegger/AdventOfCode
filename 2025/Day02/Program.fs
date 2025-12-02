open Checked
open System.IO

let parseRange (text: string) =
    let parts = text.Split('-')
    let a = int64 parts.[0]
    let b = int64 parts.[1]
    [a..b]

let parseRanges (text: string) =
    text.Split(',')
    |> Seq.collect parseRange

let isRepeatedTwice (text: string) =
    let half = text.[0..text.Length / 2 - 1]
    text = $"%s{half}%s{half}"

File.ReadAllText "input.txt"
|> parseRanges
|> Seq.filter (string >> isRepeatedTwice)
|> Seq.sum
|> printfn "Part 1: %d"

let isRepeatedAtLeastTwice (text: string) =
    [1..text.Length / 2]
    |> List.exists (fun partLength ->
        let part = text.[0..partLength - 1]
        let repeated = String.replicate (text.Length / partLength) part
        text = repeated
    )

File.ReadAllText "input.txt"
|> parseRanges
|> Seq.filter (string >> isRepeatedAtLeastTwice)
|> Seq.sum
|> printfn "Part 2: %d"
