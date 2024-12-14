open Checked
open System.IO
open System.Text.RegularExpressions

let (width, height) = (101, 103) // (11, 7)

let parseLine (line: string) =
    let m = Regex.Match(line, @"^p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
    ((int m.Groups.[1].Value, int m.Groups.[2].Value), (int m.Groups.[3].Value, int m.Groups.[4].Value))

let parse lines =
    lines
    |> Seq.map parseLine
    |> Seq.toList

let wrap v max =
    (v % max + max) % max

let step ((x, y), (vx, vy)) =
    ((wrap (x + vx) width, wrap (y + vy) height), (vx, vy))

let stepAll = List.map step

let move steps robots =
    [1..steps]
    |> List.fold (fun robots i -> stepAll robots) robots

let calculateSafetyFactor robots =
    (robots |> List.filter (fun ((x, y), _) -> x < width / 2 && y < height / 2) |> List.length) *
    (robots |> List.filter (fun ((x, y), _) -> x > width / 2 && y < height / 2) |> List.length) *
    (robots |> List.filter (fun ((x, y), _) -> x < width / 2 && y > height / 2) |> List.length) *
    (robots |> List.filter (fun ((x, y), _) -> x > width / 2 && y > height / 2) |> List.length)

File.ReadAllLines "input.txt"
|> parse
|> move 100
|> calculateSafetyFactor
|> printfn "Part 1: %d"

let draw robots =
    let positions = robots |> List.map fst |> Set.ofList
    [
        for y in 0..height-1 do
        [
            for x in 0..width-1 do
                if Set.contains (x, y) positions then "X" else "."
        ]
    ]
    |> List.map (String.concat "")
    |> String.concat "\n"
    |> printfn "%s"

let getNeighbors (x, y) = Set.ofList [ (x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1) ]

let isChristmasTree robots =
    let positions = robots |> List.map fst |> Set.ofList
    let robotsWithNeighbors = positions |> Set.filter (fun p -> Set.intersect (getNeighbors p) positions |> Set.isEmpty |> not)
    Set.count robotsWithNeighbors > positions.Count / 2

let move' robots =
    let rec fn i robots =
        if isChristmasTree robots then
            // draw robots
            i
        else fn (i + 1) (stepAll robots)
    fn 0 robots

File.ReadAllLines "input.txt"
|> parse
|> move'
|> printfn "Part 2: %d"
