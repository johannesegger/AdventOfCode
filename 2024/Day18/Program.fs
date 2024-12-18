open Checked
open System.IO

let parsePosition (line: string) =
    let parts = line.Split(',')
    (int parts.[0], int parts.[1])

let parse lines =
    lines
    |> Seq.map parsePosition
    |> Seq.toList

let maxPosition = (70, 70)

let getNeighbors (x, y) =
    [
        x - 1, y
        x, y - 1
        x + 1, y
        x, y + 1
    ]
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x <= fst maxPosition && y <= snd maxPosition)

let findPath bytes =
    let rec fn positions visitedPositions steps =
        if Set.contains maxPosition positions then Some steps
        elif Set.isEmpty positions then None
        else
            let nextPositions =
                positions
                |> Seq.collect getNeighbors
                |> Seq.filter (fun p -> not <| Set.contains p visitedPositions && not (List.contains p bytes))
                |> Set.ofSeq
            let visitedPositions' = Set.union visitedPositions nextPositions
            fn nextPositions visitedPositions' (steps + 1)

    fn (Set.singleton (0, 0)) Set.empty 0

File.ReadAllLines "input.txt"
|> parse
|> List.take 1024
|> findPath
|> Option.get
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parse
|> fun bytes ->
    [bytes.Length - 1..-1..1024]
    |> List.find (fun l ->
        bytes.[..l] |> findPath |> Option.isSome
    )
    |> fun i -> List.item (i + 1) bytes
|> fun (x, y) -> printfn $"Part 2: %d{x},%d{y}"
