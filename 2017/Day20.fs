type Vector = {
    X: int64
    Y: int64
    Z: int64
}

module Vector =
    let length v =
        v.X * v.X + v.Y * v.Y + v.Z * v.Z |> float |> System.Math.Sqrt
    let add v1 v2 =
        {
            X = v1.X + v2.X
            Y = v1.Y + v2.Y
            Z = v1.Z + v2.Z
        }

type Particle = {
    Index: int
    Position: Vector
    Velocity: Vector
    Acceleration: Vector
}

let parseLine idx line =
    let numberPattern name =
        sprintf @"(?<%s>-?\d+)" name
    let vectorPattern name =
        let numberPattern = sprintf "%s_%s" name >> numberPattern
        sprintf @"<%s,%s,%s>" (numberPattern "x") (numberPattern "y") (numberPattern "z")
    let pattern =
        sprintf @"^p=%s,\s*v=%s,\s*a=%s$" (vectorPattern "p") (vectorPattern "v") (vectorPattern "a")
    let m = System.Text.RegularExpressions.Regex.Match(line, pattern)
    if not m.Success then failwithf "Line can't be parsed: '%s' '%s'" pattern line

    let value (name: string) =
        int64 m.Groups.[name].Value

    {
        Index = idx
        Position = { X = value "p_x"; Y = value "p_y"; Z = value "p_z" }
        Velocity = { X = value "v_x"; Y = value "v_y"; Z = value "v_z" }
        Acceleration = { X = value "a_x"; Y = value "a_y"; Z = value "a_z" }
    }

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day20-input.txt")
    |> Seq.mapi parseLine
    |> Seq.toList

let solution1 =
    input
    |> Seq.groupBy ((fun p -> p.Acceleration) >> Vector.length)
    |> Seq.minBy fst
    |> snd
    |> Seq.exactlyOne
    |> fun p -> p.Index

let solution2 =
    let update particles =
        particles
        |> List.map (fun p ->
            let velocity = Vector.add p.Velocity p.Acceleration
            { p with
                Position = Vector.add p.Position velocity
                Velocity = velocity
            }
        )

    let removeCollisions p =
        p
        |> List.groupBy (fun p -> p.Position)
        |> List.choose (snd >> (function | [p] -> Some p | _ -> None))

    Seq.initInfinite ignore
    |> Seq.scan (fun p () -> update p |> removeCollisions) input
    |> Seq.item 100_000 // just a bold guess - but certinaly not always true - that after that many iterations particles won't collide any more
    |> List.length
