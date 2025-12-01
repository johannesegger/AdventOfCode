open Checked
open System.IO

let parseRotation (v: string) =
    if v.StartsWith 'L' then int v.[1..] * -1
    else int v.[1..]

let getDialPoints start rotations =
    ((start, [start]), rotations)
    ||> Seq.fold (fun (point, points) rotation ->
        let newPoint = point + rotation
        newPoint, newPoint :: points
    )
    |> snd
    |> Seq.rev

File.ReadAllLines "input.txt"
|> Seq.map parseRotation
|> getDialPoints 50
|> Seq.filter (fun p -> p % 100 = 0)
|> Seq.length
|> printfn "Part 1: %d"

let getZeroCrossings (a: int, b: int) =
    let (a, b) =
        if a < 0 || b < 0 then
            // ensure a and b are >= 0
            let offset = (max (-a / 100) (-b / 100) + 1) * 100
            (a + offset, b + offset)
        else (a, b)
    let dir = if b > a then +1 else -1
    let a' = if a % 100 = 0 then a + dir else a
    let b' = if b % 100 = 0 then b - dir else b
    abs (b' / 100 - a' / 100)

File.ReadAllLines "input.txt"
|> Seq.map parseRotation
|> getDialPoints 50
|> Seq.pairwise
|> Seq.sumBy (fun (a, b) ->
    getZeroCrossings (a, b) + (if b % 100 = 0 then 1 else 0)
)
|> printfn "Part 2: %d"
