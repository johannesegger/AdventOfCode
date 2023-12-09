open System.IO
open System.Text.RegularExpressions

let parseCoordinate (line: string) =
    let parts = line.Split(",")
    (int parts.[0], int parts.[1])

type Fold = FoldLeft of int | FoldUp of int

let parseFoldInstruction line =
    let m = Regex.Match(line, @"^fold along (x|y)=(\d+)$")
    let coordinate = int m.Groups.[2].Value
    if m.Groups.[1].Value = "x" then FoldLeft coordinate
    else FoldUp coordinate

let parseSheet (lines: string array) =
    let dots =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.map parseCoordinate
        |> Set.ofSeq

    let foldInstructions =
        lines
        |> Seq.skipWhile ((<>) "")
        |> Seq.skip 1
        |> Seq.map parseFoldInstruction
        |> Seq.toList

    (dots, foldInstructions)

let fold dots = function
    | FoldLeft foldPosition ->
        dots
        |> Set.map (fun (x, y) ->
            if x > foldPosition then (2 * foldPosition - x, y)
            else (x, y)
        )
    | FoldUp foldPosition ->
        dots
        |> Set.map (fun (x, y) ->
            if y > foldPosition then (x, 2 * foldPosition - y)
            else (x, y)
        )

File.ReadAllLines "input.txt"
|> parseSheet
|> fun (dots, foldInstructions) -> fold dots (List.head foldInstructions)
|> Set.count
|> printfn "Part 1: %d"

let printDots dots =
    let maxX = dots |> Set.map fst |> Set.maxElement
    let maxY = dots |> Set.map snd |> Set.maxElement
    [
        for y in 0 .. maxY do
        [
            for x in 0 .. maxX do
                if Set.contains (x, y) dots then "#" else "."
        ]
        |> String.concat ""
    ]
    |> String.concat "\n"

let foldAll (dots, foldInstructions) =
    (dots, foldInstructions)
    ||> List.fold fold

File.ReadAllLines "input.txt"
|> parseSheet
|> foldAll
|> printDots
|> printfn "Part 2: \n%s"
