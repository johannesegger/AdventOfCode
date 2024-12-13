open Checked
open System.IO
open System.Text.RegularExpressions

let parseGame (lines: string[]) =
    let m = Regex.Match(lines.[0], @"^Button A: X\+(\d+), Y\+(\d+)$")
    let buttonA = (int m.Groups.[1].Value, int m.Groups.[2].Value)
    let m = Regex.Match(lines.[1], @"^Button B: X\+(\d+), Y\+(\d+)$")
    let buttonB = (int m.Groups.[1].Value, int m.Groups.[2].Value)
    let m = Regex.Match(lines.[2], @"^Prize: X=(\d+), Y=(\d+)$")
    let prize = (int m.Groups.[1].Value, int m.Groups.[2].Value)
    (buttonA, buttonB, prize)

let parse lines =
    lines
    |> Array.chunkBySize 4
    |> Array.map parseGame

let getMinTokens ((ax, ay), (bx, by), (px, py)) =
    [
        for i in 0..100 do
            let xDiff = px - ax * i
            let yDiff = py - ay * i
            if xDiff > 0 && yDiff > 0 && xDiff % bx = 0 && yDiff % by = 0 && xDiff / bx = yDiff / by then
                let j = xDiff / bx
                i * 3 + j
    ]
    |> function
    | [] -> 0
    | v -> List.min v

File.ReadAllLines "input.txt"
|> parse
|> Array.sumBy getMinTokens
|> printfn "Part 1: %A"

let tryGetPressCount (ax, ay) (bx, by) (px, py) =
    // Solve i in `(px - i * ax) / bx = (py - i * ay) / by`
    if (py * int64 bx - px * int64 by) % (int64 ay * int64 bx - int64 ax * int64 by) = 0L then
        Some ((py * int64 bx - px * int64 by) / (int64 ay * int64 bx - int64 ax * int64 by))
    else None

let getPressCounts (ax, bx, px) buttonAPressCount =
    (buttonAPressCount, (int64 px - int64 ax * buttonAPressCount) / int64 bx)

let getCosts (pressCountA, pressCountB) =
    pressCountA * 3L + pressCountB

let getMinTokens' ((ax, ay), (bx, by), (px, py)) =
    tryGetPressCount (ax, ay) (bx, by) (px, py)
    |> Option.map (getPressCounts (ax, bx, px) >> getCosts)
    |> Option.defaultValue 0L

File.ReadAllLines "input.txt"
|> parse
|> Array.map (fun (a, b, (px, py)) -> (a, b, (int64 px + 10000000000000L, int64 py + 10000000000000L)))
|> Array.sumBy getMinTokens'
|> printfn "Part 2: %d"
