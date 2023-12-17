open Checked
open System.IO

let parseMap (lines: string array) =
    Array2D.init lines.Length lines.[0].Length (fun row col ->
        lines.[row].[col] - '0' |> int
    )

type Direction = Up | Down | Right | Left

let getOtherDirections = function
    | Up | Down -> [ Left; Right ]
    | Left | Right -> [ Up; Down ]

let moveInDirection (row, col) = function
    | Up -> (row - 1, col)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)

let getAt (row, col) (map: _ array2d) =
    map.[row, col]

let isOnMap (row, col) map =
    row >= 0 && row < Array2D.length1 map && col >= 0 && col < Array2D.length2 map

let getNextPositions map (newMinSteps, newMaxSteps) (position, (direction, (minSteps, maxSteps), heatLoss)) =
    [
        if maxSteps > 0 then yield direction
        if minSteps = 0 then yield! getOtherDirections direction
    ]
    |> List.choose (fun newDirection ->
        let nextPosition = moveInDirection position newDirection
        if isOnMap nextPosition map then
            let stepsLeft =
                if newDirection = direction then (max (minSteps - 1) 0, maxSteps - 1)
                else (newMinSteps - 1, newMaxSteps - 1)
            Some (nextPosition, (newDirection, stepsLeft, heatLoss + getAt nextPosition map))
        else None
    )

let inline hasBetterEntry (direction, (minSteps, maxSteps), heatLoss) other =
    other
    |> Seq.exists (fun (_, (min, max), h) -> min <= minSteps && h <= heatLoss)
    &&
    other
    |> Seq.exists (fun (_, (min, max), h) -> max >= maxSteps && h <= heatLoss)

let updateHeatLossEntries otherEntries entry =
    let keepNewEntry = not <| hasBetterEntry entry otherEntries
    if keepNewEntry then
        let list = (entry :: otherEntries)
        list
        |> List.filter (fun entry ->
            let other = list |> Seq.except [ entry ]
            not <| hasBetterEntry entry other
        )
        |> Some
    else None

let findWayWithMinimalHeatLoss (minSteps, maxSteps) map =
    let mutable minHeatLossMap = Map.empty
    let destinationPosition = (Array2D.length1 map - 1, Array2D.length2 map - 1)

    let rec fn positions =
        match positions with
        | [] ->
            minHeatLossMap
            |> Map.filter (fun (p, _) _ -> p = destinationPosition)
            |> Map.values
            |> Seq.collect id
            |> Seq.filter (fun (_, (minSteps, _), _) -> minSteps = 0)
            |> Seq.map (fun (_, _, heatLoss) -> heatLoss)
            |> Seq.min
        | _ ->
            let nextPositions =
                positions
                |> Seq.collect (getNextPositions map (minSteps, maxSteps))
                |> Seq.filter (fun (p, (d, s, h)) ->
                    let otherEntries = Map.tryFind (p, d) minHeatLossMap |> Option.defaultValue []
                    match updateHeatLossEntries otherEntries (d, s, h) with
                    | Some entries ->
                        minHeatLossMap <- Map.add (p, d) entries minHeatLossMap
                        true
                    | None -> false
                )
                |> Seq.toList
            fn nextPositions

    fn [ ((0, 0), (Right, (minSteps, maxSteps), 0)); ((0, 0), (Down, (minSteps, maxSteps), 0)) ]

File.ReadAllLines "input.txt"
|> parseMap
|> findWayWithMinimalHeatLoss (1, 3)
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parseMap
|> findWayWithMinimalHeatLoss (4, 10)
|> printfn "Part 2: %d"
