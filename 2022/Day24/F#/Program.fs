open System.IO

let parseBlizzardDirection = function
    | '^' -> Some (0, -1)
    | 'v' -> Some (0, 1)
    | '<' -> Some (-1, 0)
    | '>' -> Some (1, 0)
    | '.' -> None
    | c -> failwith $"Invalid blizzard direction: \"%c{c}\""

let parseMap (lines: string array) =
    lines
    |> Seq.skip 1
    |> Seq.take (lines.Length - 2)
    |> Seq.map (fun line ->
        line.Substring(1, line.Length - 2)
        |> Seq.map (parseBlizzardDirection >> Option.toList)
        |> Seq.toArray
    )
    |> Seq.toArray

module Coords =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

module Map =
    let at (x, y) (map: _[][]) =
        map.[(y % map.Length + map.Length) % map.Length].[(x % map.[0].Length + map.[0].Length) % map.[0].Length]
    let width (map: _[][]) = map.[0].Length
    let height (map: _[][]) = map.Length

let moveBlizzards (map: _[][]) =
    [|
        for y in [0..Map.height map - 1] do
            [|
                for x in [0..Map.width map - 1] ->
                    [
                        Coords.add (x, y) (0, -1)
                        Coords.add (x, y) (0, 1)
                        Coords.add (x, y) (-1, 0)
                        Coords.add (x, y) (1, 0)
                    ]
                    |> List.collect (fun p ->
                        Map.at p map
                        |> List.filter (fun d ->
                            Coords.add p d = (x, y)
                        )
                    )
            |]
    |]

let getNextStates map (x, y) =
    [
        if y < 0 then
            (x, y)
            (x, 0)
        elif y >= Map.height map then
            (x, y)
            (x, Map.height map - 1)
        else
            if Map.at (x, y) map |> List.isEmpty then (x, y)
            if x > 0 && Map.at (x - 1, y) map |> List.isEmpty then (x - 1, y)
            if x < Map.width map - 1 && Map.at (x + 1, y) map |> List.isEmpty then (x + 1, y)
            if y > 0 && Map.at (x, y - 1) map |> List.isEmpty then (x, y - 1)
            if y < Map.height map - 1 && Map.at (x, y + 1) map |> List.isEmpty then (x, y + 1)
    ]

let distanceTo (x1: int, y1: int) (x2, y2) =
    System.Math.Abs(x2 - x1) + System.Math.Abs(y2 - y1)

let findPath map source target =
    let rec fn map stepCount states =
        if List.isEmpty states then failwith "Search exhausted"
        elif states |> List.exists (fun s -> s |> distanceTo target <= 1) then
            (moveBlizzards map, stepCount + 1)
        else
            let map' = moveBlizzards map
            states
            |> List.collect (getNextStates map')
            |> List.distinct
            |> fn map' (stepCount + 1)

    fn map 0 [source]

let initialMap =
    File.ReadAllLines("input.txt")
    |> parseMap

let source = (0, -1)
let target = (Map.width initialMap - 1, Map.height initialMap)

findPath initialMap source target
|> snd
|> printfn "Part 1: %d"

((initialMap, 0), [(source, target); (target, source); (source, target)])
||> List.fold (fun (map, stepCount) (source, target) ->
    let (map', stepCount') = findPath map source target
    (map', stepCount + stepCount')
)
|> snd
|> printfn "Part 2: %d"
