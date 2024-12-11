open Checked
open System.IO

let parse (line: string) =
    line.Split() |> Seq.map int64 |> Seq.toList

let blink map (stone, count) =
    let inc = Option.defaultValue 0L >> ((+)count) >> Some
    if stone = 0L then map |> Map.change 1L inc
    else
        let s = $"%d{stone}"
        if s.Length % 2 = 0 then
            let n1 = int64 (s.[..s.Length / 2 - 1])
            let n2 = int64 (s.[s.Length / 2..])
            map
                |> Map.change n1 inc
                |> Map.change n2 inc
        else map |> Map.change (stone * 2024L) inc

let blinkAll count stones =
    let rec fn count stones =
        if count = 0 then stones
        else
            let stones = stones |> Map.toSeq |> Seq.fold blink Map.empty
            fn (count - 1) stones
    fn count (stones |> List.map (fun v -> (v, 1L)) |> Map.ofList)

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> blinkAll 25
|> Map.values
|> Seq.sum
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> blinkAll 75
|> Map.values
|> Seq.sum
|> printfn "Part 2: %d"
