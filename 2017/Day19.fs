type PathCharacter =
    | Letter of char
    | Cross
    | Vertical
    | Horizontal

type Position = {
    X: int
    Y: int
}

type Direction = North | East | South | West

type Data = {
    Route: Position list
    Direction: Direction
}

let toPathCharacter = function
    | c when c >= 'A' && c <= 'Z' -> Letter c |> Some
    | '+' -> Some Cross
    | '|' -> Some Vertical
    | '-' -> Some Horizontal
    | _ -> None

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day19-input.txt")
    |> Seq.indexed
    |> Seq.collect (fun (row, line) ->
        line
        |> Seq.indexed
        |> Seq.choose (fun (column, char) ->
            toPathCharacter char
            |> Option.map (fun c -> { X = column; Y = row }, c)
        )
    )
    |> Map.ofSeq


let defaultData =
    let startPosition =
        let isVertical = function
            | Vertical -> true
            | _ -> false
        input
        |> Map.toSeq
        |> Seq.filter (fun (p, c) -> p.Y = 0 && isVertical c)
        |> Seq.exactlyOne
        |> fst
    {
        Route = [ startPosition ]
        Direction = South
    }

let walkStraight position = function
    | North -> { position with Y = position.Y - 1 }
    | East -> { position with X = position.X + 1 }
    | South -> { position with Y = position.Y + 1 }
    | West -> { position with X = position.X - 1 }

let turn position = function
    | North
    | South ->
        let west =
            Map.tryFindKey (fun key _ -> key = { position with X = position.X - 1 }) input
            |> Option.map (fun p -> p, West)
            |> Option.toList
        let east =
            Map.tryFindKey (fun key _ -> key = { position with X = position.X + 1 }) input
            |> Option.map (fun p -> p, East)
            |> Option.toList
        west @ east |> List.exactlyOne
    | East
    | West ->
        let north =
            Map.tryFindKey (fun key _ -> key = { position with Y = position.Y - 1 }) input
            |> Option.map (fun p -> p, North)
            |> Option.toList
        let south =
            Map.tryFindKey (fun key _ -> key = { position with Y = position.Y + 1 }) input
            |> Option.map (fun p -> p, South)
            |> Option.toList
        north @ south |> List.exactlyOne

let walk data =
    let position = List.head data.Route
    let (nextPosition, direction) =
        match Map.find position input with
        | Letter _
        | Horizontal
        | Vertical -> walkStraight position data.Direction, data.Direction
        | Cross -> turn position data.Direction
    {
        Route = nextPosition :: data.Route
        Direction = direction
    }

let collectLetters route =
    route
    |> List.choose (fun p ->
        Map.find p input
        |> function
        | Letter c -> Some c
        | _ -> None
    )
    |> List.rev
    |> List.toArray
    |> System.String

let solution1 =
    Seq.initInfinite ignore
    |> Seq.scan (fun data () -> walk data) defaultData
    |> Seq.find (fun data -> Map.containsKey (List.head data.Route) input |> not)
    |> fun data -> List.tail data.Route
    |> collectLetters

let solution2 =
    Seq.initInfinite ignore
    |> Seq.scan (fun data () -> walk data) defaultData
    |> Seq.find (fun data -> Map.containsKey (List.head data.Route) input |> not)
    |> fun data -> List.tail data.Route |> List.length
