open Checked
open System.IO

let parseLine (v: string) =
    v |> Seq.map (fun v -> int (v - '0')) |> Seq.toList

let getLargestJoltage countDigits digits =
    let rec fn countDigits digits acc =
        if countDigits = 0 then acc
        else
            let nextDigit = digits |> Seq.take (List.length digits - countDigits + 1) |> Seq.max
            let nextDigitIndex = digits |> List.findIndex ((=) nextDigit)
            let remainingDigits = digits |> List.skip (nextDigitIndex + 1)
            fn (countDigits - 1) remainingDigits (acc * 10L + int64 nextDigit)

    fn countDigits digits 0

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.sumBy (getLargestJoltage 2)
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.sumBy (getLargestJoltage 12)
|> printfn "Part 2: %d"
