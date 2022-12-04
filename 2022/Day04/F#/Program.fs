open System.IO

let parse (line: string) =
    let parts = line.Split(',') |> Array.map (fun v -> v.Split('-') |> Array.map int)
    (parts.[0].[0], parts.[0].[1]),
    (parts.[1].[0], parts.[1].[1])

let input =
    File.ReadLines("input.txt")
    |> Seq.map parse
    |> Seq.toList

input
|> Seq.filter (fun ((x1, x2), (y1, y2)) ->
    (x1 <= y1 && x2 >= y2) || (y1 <= x1 && y2 >= x2)
)
|> Seq.length
|> printfn "Part 1: %A"

input
|> Seq.filter (fun ((x1, x2), (y1, y2)) ->
    (x1 <= y2 && x2 >= y1) || (y1 <= x2 && y2 >= x1)
)
|> Seq.length
|> printfn "Part 2: %A"
