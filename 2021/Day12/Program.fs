open System.IO

let parsePaths (lines: string array) =
    lines
    |> Seq.collect (fun v ->
        let parts = v.Split('-')
        [ (parts.[0], parts.[1]); (parts.[1], parts.[0]) ]
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (fromCave, toCaves) -> (fromCave, toCaves |> Seq.map snd |> Seq.toList))
    |> Map.ofSeq

let (|BigCave|SmallCave|) (input: string) = if System.Char.IsUpper input.[0] then BigCave else SmallCave

let canMoveToSmallCaveExactlyOnce path targetCave =
    match targetCave with
    | BigCave -> true
    | SmallCave -> path |> List.contains targetCave |> not

let addPossibleCaves canMoveToCave paths = function
    | cave :: rest ->
        Map.tryFind cave paths
        |> Option.defaultValue []
        |> List.filter (canMoveToCave (cave :: rest))
        |> List.map (fun nextCave -> nextCave :: cave :: rest)
    | [] -> [ [ "start" ] ]

let findWays canMoveToCave paths =
    let rec fn currentPaths allPaths =
        if List.isEmpty currentPaths then allPaths
        else
            let (finishedPaths, currentPaths') =
                currentPaths
                |> List.collect (addPossibleCaves canMoveToCave paths)
                |> List.partition (List.head >> (=) "end")
            fn currentPaths' (finishedPaths @ allPaths)
    fn [ [] ] []

File.ReadAllLines "input.txt"
|> parsePaths
|> findWays canMoveToSmallCaveExactlyOnce
|> List.length
|> printfn "Part 1: %d"

let canMoveToOneSmallCaveMultipleTimes path targetCave =
    match targetCave with
    | BigCave -> true
    | SmallCave when targetCave = "start" -> false
    | SmallCave ->
        let hasOtherSmallCaveVisitedMultipleTimes =
            path
            |> List.filter ((<>) targetCave)
            |> List.filter (function | SmallCave -> true | BigCave -> false)
            |> List.groupBy id
            |> List.exists (fun (cave, visits) -> visits.Length > 1)
        let targetCaveVisitCount =
            path
            |> List.filter ((=) targetCave)
            |> List.length
        if targetCaveVisitCount = 0 then true
        elif targetCaveVisitCount = 1 then not hasOtherSmallCaveVisitedMultipleTimes
        else false

File.ReadAllLines "input.txt"
|> parsePaths
|> findWays canMoveToOneSmallCaveMultipleTimes
|> List.length
|> printfn "Part 2: %d"
