open Checked
open System.IO

type Direction = Up | Down | Left | Right

let parseDirection = function
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | v -> failwith $"Invalid direction: %s{v}"

let parseDigPlan (line: string) =
    let parts = line.Split()
    (parseDirection parts.[0], int64 parts.[1])

let parseDirectionFromColor = function
    | '0' -> Right
    | '1' -> Down
    | '2' -> Left
    | '3' -> Up
    | v -> failwith $"Invalid direction: %c{v}"

let getStepsFromHexNumber number = System.Convert.ToInt64(number, fromBase = 16)

let parseDigPlanWithColors (line: string) =
    let color = line.Split().[2].Trim('(', ')', '#')
    (parseDirectionFromColor color.[color.Length - 1], getStepsFromHexNumber color.[0..4])

let dig (x, y) = function
    | (Up, steps) -> (x, y - steps)
    | (Down, steps) -> (x, y + steps)
    | (Left, steps) -> (x - steps, y)
    | (Right, steps) -> (x + steps, y)

let getBorder plan =
    ((0L, 0L), plan)
    ||> Seq.mapFold (fun (x, y) instruction ->
        ((x, y), dig (x, y) instruction)
    )
    |> fst
    |> fun v -> Seq.append v [ (0L, 0L) ]
    |> Seq.pairwise
    |> Seq.toList

let getTrapezoidArea ((x1, y1), (x2, y2)) =
    (y1 + y2) * (x1 - x2) / 2L

let getLength ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

// see https://en.wikipedia.org/wiki/Shoelace_formula
let getArea lines =
    let areaInside = lines |> List.sumBy getTrapezoidArea
    let perimeter = lines |> List.sumBy getLength
    areaInside + perimeter / 2L + 1L

File.ReadAllLines "sample.txt"
|> Seq.map parseDigPlan
|> getBorder
|> getArea
|> printfn "Part 1: %d"

File.ReadAllLines "sample.txt"
|> Seq.map parseDigPlanWithColors
|> getBorder
|> getArea
|> printfn "Part 2: %d"
