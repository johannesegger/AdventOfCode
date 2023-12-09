open System.IO

let parseRiskLevelMap (lines: string array) =
    Array2D.init lines.Length lines.[0].Length (fun row col ->
        lines.[row].[col] - '0' |> int
    )

let getTargetPosition (map: int array2d) =
    (map.GetLength 0 - 1, map.GetLength 1 - 1)

let getRiskLevel (map: int array2d) (x, y) =
    map.[y, x]

let getNeighborPositions (map: int array2d) (x, y) =
    [
        (x - 1, y)
        (x + 1, y)
        (x, y - 1)
        (x, y + 1)
    ]
    |> List.filter (fun (x, y) -> x >= 0 && x < map.GetLength 1 && y >= 0 && y < map.GetLength 0)

let updateRiskLevel (map: int array2d) ((x, y), riskLevel) =
    map.[y, x] <- riskLevel
    map

let getNeighborPositionsWithSmallerRiskLevels (map, minRiskLevels) positions =
    positions
    |> List.collect (fun (position, risk) ->
        getNeighborPositions map position
        |> List.map (fun next -> (next, risk + getRiskLevel map next))
    )
    |> List.groupBy fst
    |> List.map (fun (position, riskLevels) -> (position, riskLevels |> List.map snd |> List.min))
    |> List.filter (fun (position, risk) ->
        risk < getRiskLevel minRiskLevels position
    )

let findLowestTotalRisk map =
    let rec fn minRiskLevels positions =
        if List.isEmpty positions then
            getTargetPosition map
            |> getRiskLevel minRiskLevels
        else
            let positions' = positions |> getNeighborPositionsWithSmallerRiskLevels (map, minRiskLevels)
            let minRiskLevels' =
                positions'
                |> List.fold updateRiskLevel minRiskLevels
            fn minRiskLevels' positions'

    let minRiskLevels = Array2D.create (map.GetLength 0) (map.GetLength 1) System.Int32.MaxValue
    fn minRiskLevels [ ((0, 0), 0) ]

File.ReadAllLines "input.txt"
|> parseRiskLevelMap
|> findLowestTotalRisk
|> printfn "Part 1: %d"

let duplicateTile (tile: int array2d) =
    let tileHeight = tile.GetLength 0
    let tileWidth = tile.GetLength 1
    Array2D.init (tileHeight * 5) (tileWidth * 5) (fun row col ->
        let add = row / tileHeight + col / tileWidth
        (tile.[row % tileHeight, col % tileWidth] + add - 1) % 9 + 1
    )

File.ReadAllLines "input.txt"
|> parseRiskLevelMap
|> duplicateTile
|> findLowestTotalRisk
|> printfn "Part 2: %d"
