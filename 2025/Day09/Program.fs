open Checked
open System.IO

let parseCorner (v: string) =
    let parts = v.Split(',')
    (int parts.[0], int parts.[1])

module List =
    let getDistinctPairs list =
        let rec fn v acc =
            match v with
            | x :: xs ->
                let pairs =
                    xs
                    |> List.map (fun y -> (x, y))
                fn xs (acc @ pairs)
            | [] -> acc
        fn list []

let getArea ((x1, y1), (x2, y2)) =
    let dx = abs(x2 - x1) + 1
    let dy = abs(y2 - y1) + 1
    int64 dx * int64 dy

let findLargestRectangle corners =
    List.getDistinctPairs corners
    |> Seq.map getArea
    |> Seq.max

File.ReadAllLines "input.txt"
|> Seq.map parseCorner
|> Seq.toList
|> findLargestRectangle
|> printfn "Part 1: %d"

let getWrappedPairs points =
    points
    |> List.append [ List.last points ]
    |> List.pairwise

type Point = {
    X: int
    Y: int
}
module Point =
    let fromTuple (x, y) = { X = x; Y = y }

type Line =
    private
    | Horizontal of x1: int * x2: int * y: int
    | Vertical of y1: int * y2: int * x: int
    static member FromPoints (p1, p2) =
        if p1.Y = p2.Y then Horizontal (min p1.X p2.X, max p1.X p2.X, p1.Y)
        elif p1.X = p2.X then Vertical (min p1.Y p2.Y, max p1.Y p2.Y, p1.X)
        else failwith $"Line %A{p1}->%A{p2} is neither horizontal nor vertical"
module Line =
    let getPoints l =
        match l with
        | Horizontal (x1, x2, y) -> ({ X = x1; Y = y }, { X = x2; Y = y })
        | Vertical (y1, y2, x) -> ({ X = x; Y = y1 }, { X = x; Y = y2 })

    let intersect l1 l2 =
        match l1, l2 with
        | Horizontal (hx1, hx2, hy), Vertical (vy1, vy2, vx)
        | Vertical (vy1, vy2, vx), Horizontal (hx1, hx2, hy) ->
            vx > min hx1 hx2 && vx < max hx1 hx2 &&
            hy > min vy1 vy2 && hy < max vy1 vy2
        | _ -> false // Not necessary to check intersections of lines with equal direction
type Rect =
    private Rect of Point * Point
        static member FromPoints (p1, p2) =
            let leftTop = { X = min p1.X p2.X; Y = min p1.Y p2.Y }
            let rightBottom = { X = max p1.X p2.X; Y = max p1.Y p2.Y }
            Rect (leftTop, rightBottom)
module Rect =
    let containsPoint (Rect (leftTop, rightBottom)) point =
        point.X > leftTop.X && point.X < rightBottom.X &&
        point.Y > leftTop.Y && point.Y < rightBottom.Y
    let intersectsWithLine rect line =
        let (a, b) = Line.getPoints line
        containsPoint rect a || containsPoint rect b
    let getLines (Rect (leftTop, rightBottom)) =
        let rightTop = { X = rightBottom.X; Y = leftTop.Y }
        let leftBottom = { X = leftTop.X; Y = rightBottom.Y }
        [
            Line.FromPoints (leftTop, rightTop)
            Line.FromPoints (rightTop, rightBottom)
            Line.FromPoints (rightBottom, leftBottom)
            Line.FromPoints (leftBottom, leftTop)
        ]
    let isCrossedByLine rect line =
        getLines rect
        |> List.exists (Line.intersect line)
    let hasIntersection rect line =
        intersectsWithLine rect line || isCrossedByLine rect line
    let getArea (Rect (leftTop, rightBottom)) =
        getArea ((leftTop.X, leftTop.Y), (rightBottom.X, rightBottom.Y))

module Visualization =
    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Drawing.Processing
    open SixLabors.ImageSharp.PixelFormats
    open SixLabors.ImageSharp.Processing

    let create corners rect =
        let (minX, maxX, minY, maxY) =
            corners |> List.map _.X |> List.min,
            corners |> List.map _.X |> List.max,
            corners |> List.map _.Y |> List.min,
            corners |> List.map _.Y |> List.max
        let (width, height) = (maxX - minX + 1, maxY - minY + 1)
        let getX x = float32 (x - minX) / 10f
        let getY y = float32 (y - minY) / 10f
        let points =
            corners
            |> List.map (fun p -> PointF(getX p.X, getY p.Y))
            |> List.toArray
        use image = new Image<Rgba32>(width / 10, height / 10)
        image.Mutate(fun x ->
            x.DrawPolygon(Pens.Solid(Color.Orange),points) |> ignore
            match rect with
            | Some (Rect (leftTop, rightBottom)) ->
                x.DrawPolygon(
                    Pens.Solid(Color.GreenYellow),
                    [|
                        PointF(getX leftTop.X, getY leftTop.Y)
                        PointF(getX rightBottom.X, getY leftTop.Y)
                        PointF(getX rightBottom.X, getY rightBottom.Y)
                        PointF(getX leftTop.X, getY rightBottom.Y)
                        |]
                ) |> ignore
            | None -> ()
        )
        image.SaveAsPng("path.png")

let isOnlyRedAndGreenTiles edges rect =
    edges
    |> List.exists (Rect.hasIntersection rect)
    |> not

let findLargestRectangleWithRedAndGreenTiles corners =
    let edges =
        corners
        |> getWrappedPairs
        |> List.map Line.FromPoints
    let maxRect =
        corners
        |> List.getDistinctPairs
        |> Seq.map Rect.FromPoints
        |> Seq.filter (isOnlyRedAndGreenTiles edges)
        |> Seq.maxBy Rect.getArea
    Visualization.create corners (Some maxRect)
    Rect.getArea maxRect

File.ReadAllLines "input.txt"
|> Seq.map (parseCorner >> Point.fromTuple)
|> Seq.toList
|> findLargestRectangleWithRedAndGreenTiles
|> printfn "Part 2: %d"
