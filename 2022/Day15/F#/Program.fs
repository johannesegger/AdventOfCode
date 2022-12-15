open System
open System.IO
open System.Text.RegularExpressions

let parseLine v =
    let m = Regex.Match(v, @"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$")
    if not m.Success then failwith $"Can't parse \"%s{v}\""
    let sensor = (int m.Groups.[1].Value, int m.Groups.[2].Value)
    let beacon = (int m.Groups.[3].Value, int m.Groups.[4].Value)
    (sensor, beacon)

let sensorMap =
    File.ReadLines("input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let getDistance (x1: int, y1: int) (x2: int, y2: int) =
    Math.Abs(x2 - x1) + Math.Abs(y2 - y1)

let (minX, maxX) =
    let horizontalRange =
        sensorMap
        |> List.collect (fun ((sx, sy), b) ->
            let d = getDistance (sx, sy) b
            [ sx - d; sx + d ]
        )
    (List.min horizontalRange, List.max horizontalRange)

let isCoveredBy (sensor, nearestBeacon) beacon =
    getDistance sensor nearestBeacon >= getDistance sensor beacon

let isCoveredByAny sensors beacon =
    sensors
    |> Seq.exists (fun s ->
        isCoveredBy s beacon
    )

let getCoverageAtLine y map =
    let sensorAndBeaconPositions =
        map
        |> Seq.collect (fun (s, b) -> [s; b])
        |> Set.ofSeq
    [for x in minX .. maxX -> (x, y)]
    |> List.filter (fun position ->
        not <| Set.contains position sensorAndBeaconPositions &&
        isCoveredByAny map position
    )
    |> List.length

sensorMap
|> getCoverageAtLine 2_000_000
|> printfn "Part 1: %d"

let getBorder ((sensorX, sensorY), beacon) =
    let borderDistance = getDistance (sensorX, sensorY) beacon + 1
    [
        for dx in [0 .. borderDistance] do
            let dy = borderDistance - dx in
            (sensorX + dx, sensorY + dy)
            (sensorX - dx, sensorY + dy)
            (sensorX + dx, sensorY - dy)
            (sensorX - dx, sensorY - dy)
    ]

// Ensuring there's only a single solution does take quite a bit longer,
// so we just find the first solution and hope it's the only one
sensorMap
|> Seq.collect getBorder
|> Seq.find (fun (x, y) ->
    x >= 0 && x <= 4_000_000 &&
    y >= 0 && y <= 4_000_000 &&
    not <| isCoveredByAny sensorMap (x, y)
)
|> fun (x, y) -> int64 x * 4_000_000L + int64 y 
|> printfn "Part 2: %d"
