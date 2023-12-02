open System
open System.IO
open System.Text.RegularExpressions

let parseLine line =
    let m = Regex.Match(line, @"Game (\d+): (.+)$")
    let gameId = m.Groups.[1].Value
    let game =
        m.Groups.[2].Value.Split(';', StringSplitOptions.TrimEntries)
        |> Seq.map (fun subset ->
            subset.Split(',', StringSplitOptions.TrimEntries)
            |> Seq.map (fun cube ->
                let parts = cube.Split(' ')
                (int parts.[0], parts.[1])
            )
            |> Seq.toList
        )
        |> Seq.toList
    (int gameId, game)

let countCubes (subset: (int * string) list) color =
    subset
    |> List.filter (fun (_count, c) -> c = color)
    |> List.sumBy fst

let isGamePossible (gameId, game) =
    game
    |> List.forall (fun subset ->
        countCubes subset "red" <= 12 && countCubes subset "green" <= 13 && countCubes subset "blue" <= 14
    )

File.ReadAllLines("input.txt")
|> Seq.map parseLine
|> Seq.filter isGamePossible
|> Seq.sumBy fst
|> printfn "Part 1: %d"

let minimumCubeCounts (gameId, game) =
    let counts =
        game
        |> List.map (fun subset ->
            (countCubes subset "red", countCubes subset "green", countCubes subset "blue")
        )
    counts |> List.map (fun (v, _, _) -> v) |> List.max,
    counts |> List.map (fun (_, v, _) -> v) |> List.max,
    counts |> List.map (fun (_, _, v) -> v) |> List.max

File.ReadAllLines("input.txt")
|> Seq.map parseLine
|> Seq.map minimumCubeCounts
|> Seq.map (fun (red, green, blue) -> red * green * blue)
|> Seq.sum
|> printfn "Part 2: %d"
