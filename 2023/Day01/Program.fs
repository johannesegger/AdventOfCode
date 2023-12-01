open System.IO

File.ReadAllLines("input.txt")
|> Seq.sumBy (fun v ->
    let first = v |> Seq.find System.Char.IsDigit |> fun v -> int (v - '0')
    let last = v |> Seq.findBack System.Char.IsDigit |> fun v -> int (v - '0')
    first * 10 + last
)
|> printfn "Part 1: %d"

let getNumbers (line: string) =
    [
        ("1", 1); ("one", 1)
        ("2", 2); ("two", 2)
        ("3", 3); ("three", 3)
        ("4", 4); ("four", 4)
        ("5", 5); ("five", 5)
        ("6", 6); ("six", 6)
        ("7", 7); ("seven", 7)
        ("8", 8); ("eight", 8)
        ("9", 9); ("nine", 9)
    ]
    |> List.collect (fun (text, digit) ->
        [
            match line.IndexOf text with
            | -1 -> ()
            | index -> yield (index, digit)

            match line.LastIndexOf text with
            | -1 -> ()
            | index -> yield (index, digit)
        ]
    )

File.ReadAllLines("input.txt")
|> Seq.sumBy (fun v ->
    let numbers = getNumbers v
    let first = numbers |> Seq.minBy fst |> snd
    let last = numbers |> Seq.maxBy fst |> snd
    first * 10 + last
)
|> printfn "Part 2: %d"
