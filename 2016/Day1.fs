type Turn = Right | Left
type Direction = North | East | South | West
type Position = { Direction: Direction; X: int; Y: int }

let initialPosition = { Direction = North; X=0; Y=0 }

let input = "R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3"
let parsedInput =
    input.Split([|", "|], System.StringSplitOptions.None)
    |> Seq.map (fun p ->
        let distance = p.Substring 1 |> int
        if p.[0] = 'R' then Right else Left
        , distance
    )

let walk p (turn, distance) =
    match p.Direction, turn with
    | North, Right -> East
    | North, Left -> West
    | East, Right -> South
    | East, Left -> North
    | South, Right -> West
    | South, Left -> East
    | West, Right -> North
    | West, Left -> South
    |> fun direction -> { p with Direction = direction }
    |> fun p ->
        match p.Direction with
        | North -> { p with Y = p.Y + distance }
        | East -> { p with X = p.X + distance }
        | South -> { p with Y = p.Y - distance }
        | West -> { p with X = p.X - distance }

let getDistance p =
    System.Math.Abs p.X + System.Math.Abs p.Y

let solution1 =
    parsedInput
    |> Seq.fold walk initialPosition
    |> getDistance

let getIntermediatePositions p1 p2 =
    match p2.Direction with
    | North -> [ for i in 1 .. (p2.Y - p1.Y) -> { p1 with Y = p1.Y + i } ]
    | East -> [ for i in 1 .. (p2.X - p1.X) -> { p1 with X = p1.X + i } ]
    | South -> [ for i in 1 .. (p1.Y - p2.Y) -> { p1 with Y = p1.Y - i } ]
    | West -> [ for i in 1 .. (p1.X - p2.X) -> { p1 with X = p1.X - i } ]

let solution2 =
    let folder (positions, firstDuplicate) position =
        let updatedFirstDuplicate =
            match firstDuplicate with
            | None ->
                if positions |> Set.contains (position.X, position.Y)
                then Some position
                else None
            | x -> x
        Set.add (position.X, position.Y) positions, updatedFirstDuplicate
    parsedInput
    |> Seq.scan walk initialPosition
    |> Seq.pairwise
    |> Seq.collect (fun (p1, p2) -> getIntermediatePositions p1 p2)
    |> Seq.append [initialPosition]
    |> Seq.fold folder (Set.empty, None)
    |> snd
    |> Option.map getDistance
    