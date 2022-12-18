open System.IO

let parsePosition (v: string) =
    let parts = v.Split(',')
    (int parts.[0], int parts.[1], int parts.[2])

let positions =
    File.ReadLines("input.txt")
    |> Seq.map parsePosition
    |> Set.ofSeq

let neighborPositions (x, y, z) =
    [
        (x + 1, y, z)
        (x - 1, y, z)
        (x, y + 1, z)
        (x, y - 1, z)
        (x, y, z + 1)
        (x, y, z - 1)
    ]

let countFreeSurfaces dropletPositions =
    dropletPositions
    |> Seq.sumBy (fun position ->
        neighborPositions position
        |> Set.ofList
        |> fun v -> Set.difference v dropletPositions
        |> Set.count
    )

positions
|> countFreeSurfaces
|> printfn "Part 1: %A"

let minX = (positions |> Seq.map (fun (x, y, z) -> x) |> Seq.min) - 1
let minY = (positions |> Seq.map (fun (x, y, z) -> y) |> Seq.min) - 1
let minZ = (positions |> Seq.map (fun (x, y, z) -> z) |> Seq.min) - 1

let maxX = (positions |> Seq.map (fun (x, y, z) -> x) |> Seq.max) + 1
let maxY = (positions |> Seq.map (fun (x, y, z) -> y) |> Seq.max) + 1
let maxZ = (positions |> Seq.map (fun (x, y, z) -> z) |> Seq.max) + 1

let allPositions =
    [
        for x in [minX..maxX] do
        for y in [minY..maxY] do
        for z in [minZ..maxZ] do
            (x, y, z)
    ]
    |> Set.ofList

let airPositions =
    let rec fn total current =
        let newPositions =
            current
            |> Seq.collect neighborPositions
            |> Seq.filter (fun (x, y, z) ->
                x >= minX && x <= maxX &&
                y >= minY && y <= maxY &&
                z >= minZ && z <= maxZ
            )
            |> Set.ofSeq
            |> fun v ->
                let v' = Set.difference v positions
                let v'' = Set.difference v' current
                Set.difference v'' total
        
        let total' = Set.union total current
        if Set.isEmpty newPositions then total'
        else fn total' newPositions
    fn Set.empty (Set.ofList [ (minX, minY, minZ) ])

Set.difference allPositions airPositions
|> countFreeSurfaces
|> printfn "Part 2: %A"
