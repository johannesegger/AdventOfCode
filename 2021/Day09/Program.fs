open System.IO

let parseLine (line: string) =
    line |> Seq.map (fun v -> v - '0' |> int) |> Seq.toArray

let getAdjacentPositions (map: int array array) (x, y) =
    [
        (x - 1, y)
        (x, y - 1)
        (x + 1, y)
        (x, y + 1)
    ]
    |> List.filter (fun (x, y) -> x >= 0 && x < map.[0].Length && y >= 0 && y < map.Length)

let findLowPoints (map: int array array) =
    [
        for y in 0..map.Length - 1 do
        for x in 0..map.[0].Length - 1 do
            let height = map.[y].[x]
            let isLowPoint =
                getAdjacentPositions map (x, y)
                |> Seq.map (fun (x, y) -> map.[y].[x])
                |> Seq.forall (fun v -> v > height)
            if isLowPoint then (x, y)
    ]

let getLowPointRiskLevelSum map =
    findLowPoints map
    |> Seq.sumBy (fun (x, y) -> map.[y].[x] + 1)

File.ReadAllLines "input.txt"
|> Array.map parseLine
|> getLowPointRiskLevelSum
|> printfn "Part 1: %d"

let getBasinSize (map: int array array) lowPoint =
    let rec fn visitedPositions =
        let visitedPositions' =
            visitedPositions
            |> Seq.collect (fun (x, y) ->
                let height = map.[y].[x]
                getAdjacentPositions map (x, y)
                |> List.filter (fun (x, y) -> map.[y].[x] < 9 && map.[y].[x] > height)
            )
            |> Set.ofSeq
            |> Set.union visitedPositions
        if visitedPositions' = visitedPositions then Set.count visitedPositions
        else fn visitedPositions'

    fn (Set.singleton lowPoint)

let getBasinSizes map =
    findLowPoints map
    |> List.map (getBasinSize map)

File.ReadAllLines "input.txt"
|> Array.map parseLine
|> getBasinSizes
|> List.sortDescending
|> List.take 3
|> List.reduce (*)
|> printfn "Part 2: %d"
