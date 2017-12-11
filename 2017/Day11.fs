type Direction = | N | NE | SE | S | SW | NW

module Direction =
    let fromString = function
        | "n" -> N
        | "ne" -> NE
        | "se" -> SE
        | "s" -> S
        | "sw" -> SW
        | "nw" -> NW
        | dir -> failwithf "Unknown direction: '%s'" dir

let input =
    System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\Day11-input.txt")
    |> fun s -> s.Split(',')
    |> Seq.map Direction.fromString

type Position = {
    X: int
    Y: int
}

let folder position = function
    | N -> { position with Y = position.Y + 2 }
    | NE -> { position with Y = position.Y + 1; X = position.X + 1 }
    | SE -> { position with Y = position.Y - 1; X = position.X + 1 }
    | S -> { position with Y = position.Y - 2 }
    | SW -> { position with Y = position.Y - 1; X = position.X - 1 }
    | NW -> { position with Y = position.Y + 1; X = position.X - 1 }

let abs p =
    { X = System.Math.Abs p.X; Y = System.Math.Abs p.Y }

let steps p =
    if p.X < p.Y
    then p.X + (p.Y - p.X) / 2
    else p.X

let solution1 =
    input
    |> Seq.fold folder { X = 0; Y = 0 }
    |> abs
    |> steps

let solution2 =
    let folder' (position, maxDistance) direction =
        let position' = folder position direction
        let distance = position' |> abs |> steps
        let maxDistance' =
            if distance > maxDistance then distance
            else maxDistance
        position', maxDistance'
    input
    |> Seq.fold folder' ({ X = 0; Y = 0 }, 0)
    |> snd
