open Checked
open System.IO

type RockType = RoundRock | SquareRock

let getSize rocks =
    let rows = rocks |> List.map (fun ((row, _), _) -> row) |> List.max
    let cols = rocks |> List.map (fun ((_, col), _) -> col) |> List.max
    (rows, cols)

let parseDish (lines: string array) =
    let rocks = [
        for row in 0..lines.Length - 1 do
        for col in 0..lines.[row].Length - 1 do
            if lines.[row].[col] = 'O' then ((row, col), RoundRock)
            elif lines.[row].[col] = '#' then ((row, col), SquareRock)
    ]
    (getSize rocks, rocks)

let tilt (size, rocks) =
    let colGroups = rocks |> List.groupBy (fun ((_row, col), _rockType) -> col) |> Map.ofList
    let rocks' =
        rocks
        |> List.map (fun ((row, col), rockType) ->
            if rockType = SquareRock then ((row, col), rockType)
            else
                let colGroup = Map.find col colGroups
                let fixRow =
                    colGroup
                    |> List.filter (fun ((otherRow, _otherCol), rockType) -> otherRow < row && rockType = SquareRock)
                    |> function
                    | [] -> -1
                    | v -> v |> List.map (fun ((row, _), _) -> row) |> List.max
                let otherRockCount =
                    colGroup
                    |> List.filter (fun ((otherRow, _otherCol), _rockType) -> otherRow > fixRow && otherRow < row)
                    |> List.length
                let newRow = fixRow + otherRockCount + 1
                ((newRow, col), rockType)
        )
    (size, rocks')

let getTotalLoad ((maxRow, _), rocks) =
    rocks
    |> List.sumBy (fun ((row, _col), rockType) ->
        if rockType = RoundRock then maxRow - row + 1
        else 0
    )

let rotateRight ((rows, cols), rocks) =
    let rocks' =
        rocks
        |> List.map (fun ((row, col), rockType) ->
            let row' = col
            let col' = rows - row
            ((row', col'), rockType)
        )
    ((cols, rows), rocks')

File.ReadAllLines "input.txt"
|> parseDish
|> tilt
|> getTotalLoad
|> printfn "Part 1: %d"

let runCycle = tilt >> rotateRight >> tilt >> rotateRight >> tilt >> rotateRight >> tilt >> rotateRight

let findCycle rocks =
    let rec fn cache i (size, rocks) =
        match Map.tryFind (Set.ofList rocks) cache with
        | Some index -> (index, i, (size, rocks))
        | None ->
            let rocks' = runCycle (size, rocks)
            let cache' = Map.add (Set.ofList rocks) i cache
            fn cache' (i + 1) rocks'
    fn Map.empty 0 rocks

let runCycles count rocks =
    let (cycleStart, cycleEnd, rocks) = findCycle rocks
    let remaining = (count - cycleEnd) % (cycleEnd - cycleStart)
    [1..remaining] |> List.fold (fun rocks _ -> runCycle rocks) rocks

File.ReadAllLines "input.txt"
|> parseDish
|> runCycles 1_000_000_000
|> getTotalLoad
|> printfn "Part 2: %d"
