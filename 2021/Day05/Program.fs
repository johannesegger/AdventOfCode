open System
open System.IO
open System.Text.RegularExpressions

let parseLine text =
    let m = Regex.Match(text, @"^(\d+),(\d+) -> (\d+),(\d+)$")
    let startPoint = (int m.Groups.[1].Value, int m.Groups.[2].Value)
    let endPoint = (int m.Groups.[3].Value, int m.Groups.[4].Value)
    (startPoint, endPoint)

let getHorizontalOrVerticalLinePoints ((x1, y1), (x2, y2)) =
    if x1 = x2 then [ for y in (min y1 y2) .. (max y1 y2) -> (x1, y) ]
    elif y1 = y2 then [ for x in (min x1 x2) .. (max x1 x2) -> (x, y1) ]
    else []

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.collect getHorizontalOrVerticalLinePoints
|> Seq.groupBy id
|> Seq.sumBy (fun (point, list) -> if Seq.length list >= 2 then 1 else 0)
|> printfn "Part 1: %d"

let getAllLinePoints ((x1, y1), (x2, y2)) =
    if x1 = x2 then [ for y in (min y1 y2) .. (max y1 y2) -> (x1, y) ]
    elif y1 = y2 then [ for x in (min x1 x2) .. (max x1 x2) -> (x, y1) ]
    else
        let deltaX = x2 - x1
        let signY = sign (y2 - y1) * sign deltaX
        [ for x in 0 .. sign deltaX .. deltaX -> (x1 + x, y1 + x * signY) ]

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.collect getAllLinePoints
|> Seq.groupBy id
|> Seq.sumBy (fun (point, list) -> if Seq.length list >= 2 then 1 else 0)
|> printfn "Part 2: %d"
