open Checked
open System.IO

let getPosition c (map: string[]) =
    List.head [
        for row in 0..map.Length - 1 do
        for col in 0..map.[row].Length - 1 do
            if map.[row].[col] = c then (row, col)
    ]

type Direction = North | East | South | West
let turns = function
    | North | South -> [ East; West ]
    | East | West -> [ North; South ]

let step (row, col) = function
    | North -> (row - 1, col)
    | East -> (row, col + 1)
    | South -> (row + 1, col)
    | West -> (row, col - 1)

let move (map: string[]) (((row, col), direction), (path, costs)) =
    if map.[row].[col] = 'E' then []
    else [
        let (row', col') = step (row, col) direction
        if map.[row'].[col'] <> '#' then (((row', col'), direction), (Set.add (row, col) path, costs + 1))
        for direction in turns direction do (((row, col), direction), (path, costs + 1000))
    ]

let solve map =
    let rec fn visitedPositions newPositions =
        if List.length newPositions = 0 then visitedPositions
        else
            let visitedPositions' =
                newPositions
                |> Seq.fold (fun positions (position, (path, costs)) ->
                    Map.change position (function
                        | Some (p, c) when c = costs -> Some (path :: p, c)
                        | Some _
                        | None -> Some ([path], costs)
                    ) positions
                ) visitedPositions
            let newPositions' =
                newPositions
                |> List.collect (move map)
                |> List.filter (fun (position, (path, costs)) ->
                    match Map.tryFind position visitedPositions' with
                    | Some (p, c) when c < costs -> false
                    | Some _
                    | None -> true
                )
            fn visitedPositions' newPositions'
        
    fn Map.empty [(getPosition 'S' map, East), (Set.empty, 0)]

let map = File.ReadAllLines "input.txt"
let (path, costs) =
    solve map
    |> Map.toSeq
    |> Seq.find (fst >> fst >> (=) (getPosition 'E' map))
    |> snd
printfn $"Part 1: %d{costs}"
printfn $"Part 2: %d{(path |> Seq.collect id |> Seq.distinct |> Seq.length) + 1}"
