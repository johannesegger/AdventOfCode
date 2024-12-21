open Checked
open System.IO

let getStartPosition (map: string[]) =
    List.exactlyOne [
        for row in 0..map.Length - 1 do
        for col in 0..map.[row].Length - 1 do
            if map.[row].[col] = 'S' then (row, col)
    ]

let isValid (map: string[]) (row, col) =
    map |> Array.tryItem row |> Option.bind (Seq.tryItem col) |> Option.isSome

let isWall (map: string[]) (row, col) =
    map.[row].[col] = '#'

let isEnd (map: string[]) (row, col) =
    map.[row].[col] = 'E'

let getPathLength pathMap source target =
    let length = Map.find target pathMap - Map.find source pathMap
    if length < 0 then None
    else Some length

let getLength (rowA, colA) (rowB, colB) =
    abs (rowA - rowB) + abs (colA - colB)

let getCheatEnds map (row, col) maxLength =
    [
        for deltaRow in -maxLength .. +maxLength do
        let colOffset = maxLength - abs deltaRow
        for deltaCol in -colOffset .. +colOffset do
            let (r, c) = (row + deltaRow, col + deltaCol)
            if isValid map (r, c) && not <| isWall map (r, c) then (r, c)
    ]

let getCheatSteps map pathMap maxLength position =
    getCheatEnds map position maxLength
    |> List.choose (fun target ->
        match getPathLength pathMap position target with
        | Some pathLength ->
            let savings = pathLength - getLength position target
            if savings >= 100 then Some (target, savings)
            else None
        | None -> None
    )

let getNeighbors map (row, col) =
    [
        (row - 1, col)
        (row, col + 1)
        (row + 1, col)
        (row, col - 1)
    ]
    |> List.filter (isValid map)

let getNextFree map p path =
    getNeighbors map p
    |> Seq.filter (fun v -> not (isWall map v) && not <| Map.containsKey v path)
    |> Seq.exactlyOne

let getPathMap map p =
    let rec fn p i acc =
        let acc' = Map.add p i acc
        if isEnd map p then acc'
        else fn (getNextFree map p acc') (i + 1) acc'
    fn p 0 Map.empty

let findCheats maxCheatLength map =
    let startPosition = getStartPosition map
    let pathMap = getPathMap map startPosition
    let path = pathMap |> Map.toList |> List.sortBy snd |> List.map fst
    path
    |> List.collect (getCheatSteps map pathMap maxCheatLength)

File.ReadAllLines "input.txt"
|> findCheats 2
|> Seq.length
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> findCheats 20
|> Seq.length
|> printfn "Part 2: %d"
