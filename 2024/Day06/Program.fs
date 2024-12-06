open Checked
open System.IO

type Direction = Up | Down | Left | Right

let getStartingPosition map =
    map |> Seq.indexed |> Seq.pick (fun (y, row) -> row |> Seq.tryFindIndex ((=) '^') |> Option.map (fun x -> (x, y)))

let isObstacle (map: string[]) (x, y) =
    map.[y].[x] = '#'

let step (map: string[]) direction (x, y) =
    let positions =
        match direction with
        | Up -> [ for y' in y-1..-1..0 -> (x, y') ]
        | Down -> [ for y' in y+1..1..map.Length-1 -> (x, y') ]
        | Left -> [ for x' in x-1..-1..0 -> (x', y) ]
        | Right -> [ for x' in x+1..1..map.[y].Length-1 -> (x', y) ]
    let positionsUntilObstacleOrEnd =
        positions
        |> List.takeWhile (not << isObstacle map)
    (positionsUntilObstacleOrEnd, positionsUntilObstacleOrEnd.Length = positions.Length)

let turnRight = function
    | Up -> Right
    | Down -> Left
    | Left -> Up
    | Right -> Down

let walk map =
    let rec fn position direction acc =
        let (positions, isEnd) = step map direction position
        let endPosition = positions |> List.tryLast |> Option.defaultValue position
        let acc' = Set.ofList [ for p in positions -> (p, direction) ] |> Set.union acc
        if isEnd then (acc', true)
        elif acc |> Set.contains (endPosition, direction) then (acc', false)
        else fn endPosition (turnRight direction) acc'
    
    let start = getStartingPosition map
    let (positions, isEnd) = fn start Up Set.empty
    (Set.add (start, Up) positions, isEnd)

File.ReadAllLines "input.txt"
|> walk
|> fst
|> Set.map fst
|> Set.count
|> printfn "Part 1: %d"

let setObstacleAt (map: string[]) (x, y) =
    map
    |> Array.updateAt y (map.[y] |> Seq.updateAt x '#' |> Seq.toArray |> System.String)

let obstacleResultsInLoop map p =
    let exited = walk (setObstacleAt map p) |> snd
    not exited

let findObstaclePositions map =
    walk map
    |> fst
    |> Set.map fst
    |> Set.remove (getStartingPosition map)
    |> Set.toList
    |> List.filter (obstacleResultsInLoop map)

File.ReadAllLines "input.txt"
|> findObstaclePositions
|> List.length
|> printfn "Part 2: %d"
