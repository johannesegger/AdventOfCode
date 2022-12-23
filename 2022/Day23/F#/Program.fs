open System.IO

let parseMap lines =
    [
        for (y, line) in Seq.indexed lines do
        for (x, c) in Seq.indexed line do
            if c = '#' then (x, y)
    ]
    |> Set.ofList

type Direction = North | South | East | West
module Direction =
    let offset = function
        | North -> (0, -1)
        | South -> (0, 1)
        | East -> (1, 0)
        | West -> (-1, 0)
    let allOffsets = function
        | North -> [ for d in -1 .. 1 do (d, -1) ]
        | South -> [ for d in -1 .. 1 do (d, 1) ]
        | East -> [ for d in -1 .. 1 do (1, d) ]
        | West -> [ for d in -1 .. 1 do (-1, d) ]

let allFree map positions =
    positions
    |> Seq.forall (fun position -> not <| Set.contains position map)

module Coords =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let allNeighborsFree map p =
    [
        yield! Direction.allOffsets North
        yield! Direction.allOffsets South
        Direction.offset East
        Direction.offset West
    ]
    |> List.map (Coords.add p)
    |> allFree map

let makeProposal directions map p =
    if allNeighborsFree map p then
        None
    else
        directions
        |> List.tryPick (fun direction ->
            Direction.allOffsets direction
            |> List.map (Coords.add p)
            |> allFree map
            |> function
            | true -> Some (p, Coords.add p (Direction.offset direction))
            | false -> None
        )

let step (directions, map) =
    let proposals =
        map
        |> Seq.choose (makeProposal directions map)
        |> Seq.fold (fun proposals (source, target) ->
            match Map.tryFind target proposals with
            | Some _ -> Map.add target None proposals
            | None -> Map.add target (Some source) proposals
        ) Map.empty
        |> Map.toList
        |> List.choose (fun (target, source) ->
            match source with
            | Some v -> Some (v, target)
            | None -> None
        )
    let map' =
        (proposals |> List.map fst |> Set.ofList)
        |> Set.difference map
        |> Set.union (proposals |> List.map snd |> Set.ofList)
    let directions' = List.tail directions @ [List.head directions]
    (directions', map')

let getEmptyPositions map =
    let minX = map |> Seq.map fst |> Seq.min
    let maxX = map |> Seq.map fst |> Seq.max
    let minY = map |> Seq.map snd |> Seq.min
    let maxY = map |> Seq.map snd |> Seq.max
    let width = maxX - minX + 1
    let height = maxY - minY + 1
    width * height - Set.count map

let map =
    File.ReadLines("input.txt")
    |> parseMap

(([North; South; West; East], map), [1..10])
||> List.fold (fun state _ -> step state)
|> snd
|> getEmptyPositions
|> printfn "Part 1: %d"

([North; South; West; East], map)
|> Seq.unfold (fun state ->
    let state' = step state
    if snd state' = snd state then None
    else Some (state', state')
)
|> Seq.length
|> fun v -> printfn "Part 2: %d" (v + 1)
