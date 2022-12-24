open System
open System.IO
open System.Text.RegularExpressions

type Tile = Void | Open | Wall

let parseMapTile = function
    | ' ' -> Void
    | '.' -> Open
    | '#' -> Wall
    | v -> failwith $"Invalid map tile: \"%c{v}\""

let parseMapRow mapWidth (v: string) =
    v.PadRight(mapWidth, ' ')
    |> Seq.map parseMapTile
    |> Seq.toArray

let parseMap (v: string) =
    let lines = v.Split("\r\n")
    let mapWidth = lines |> Seq.map (fun l -> l.Length) |> Seq.max
    lines
    |> Seq.map (parseMapRow mapWidth)
    |> Seq.toArray

type Turn = TurnLeft | TurnRight

type Instruction =
    | ChangeDirection of Turn
    | Go of int

let parseInstruction v =
    if v = "L" then ChangeDirection TurnLeft
    elif v = "R" then ChangeDirection TurnRight
    else
        match Int32.TryParse(v) with
        | (true, v) -> Go v
        | (false, _) -> failwith "Can't parse instruction: \"%s{v}\""

let parseInstructions (v: string) =
    Regex.Matches(v, @"\d+|L|R")
    |> Seq.map (fun m -> parseInstruction m.Value)
    |> Seq.toList

let (map, instructions) =
    let content = File.ReadAllText("input.txt")
    let parts = content.Split("\r\n\r\n")
    parseMap parts.[0], parseInstructions parts.[1]

let (mapWidth, mapHeight) = (map.[0].Length, map.Length)
let faceLength = 50

let startX = map.[0] |> Array.findIndex ((=) Open)

type Direction = Left | Right | Up | Down
module Direction =
    let change turn direction =
        match turn, direction with
        | TurnLeft, Left -> Down
        | TurnLeft, Right -> Up
        | TurnLeft, Up -> Left
        | TurnLeft, Down -> Right
        | TurnRight, Left -> Up
        | TurnRight, Right -> Down
        | TurnRight, Up -> Right
        | TurnRight, Down -> Left
    let toInt = function
        | Left -> 2
        | Right -> 0
        | Up -> 3
        | Down -> 1

let getTile (x, y) =
    map.[y].[x]

let tryGoLeft (x, y) =
    let x' =
        [ x - 1 .. -1 .. 0] @ [ mapWidth - 1 .. -1 .. x ]
        |> List.find (fun x -> map.[y].[x] <> Void)
    match getTile (x', y) with
    | Void -> failwith "Still standing on Void?"
    | Open -> Some (x', y)
    | Wall -> None

let tryGoRight (x, y) =
    let x' =
        [ x + 1 .. mapWidth - 1] @ [ 0 .. x ]
        |> List.find (fun x -> map.[y].[x] <> Void)
    match getTile (x', y) with
    | Void -> failwith "Still standing on Void?"
    | Open -> Some (x', y)
    | Wall -> None

let tryGoUp (x, y) =
    let y' =
        [ y - 1 .. -1 .. 0] @ [ mapHeight - 1 .. -1 .. y ]
        |> List.find (fun y -> map.[y].[x] <> Void)
    match getTile (x, y') with
    | Void -> failwith "Still standing on Void?"
    | Open -> Some (x, y')
    | Wall -> None

let tryGoDown (x, y) =
    let y' =
        [ y + 1 .. mapHeight - 1] @ [ 0 .. y ]
        |> List.find (fun y -> map.[y].[x] <> Void)
    match getTile (x, y') with
    | Void -> failwith "Still standing on Void?"
    | Open -> Some (x, y')
    | Wall -> None

let tryStep direction position =
    match direction with
    | Left -> tryGoLeft position
    | Right -> tryGoRight position
    | Up -> tryGoUp position
    | Down -> tryGoDown position

let rec walk ((x, y), direction) = function
    | ChangeDirection (TurnLeft as turn)
    | ChangeDirection (TurnRight as turn) -> ((x, y), Direction.change turn direction)
    | Go 0 -> ((x, y), direction)
    | Go steps ->
        match tryStep direction (x, y) with
        | Some p -> walk (p, direction) (Go (steps - 1))
        | None -> ((x, y), direction)

let getResult ((x, y), direction) =
    (x + 1) * 4 + (y + 1) * 1000 + Direction.toInt direction

(((startX, 0), Right), instructions)
||> List.fold walk
|> getResult
|> printfn "Part 1: %d"

let offsetAtFace v = v % faceLength
let topEdgeOfFace (faceX, faceY) = faceY * 50
let bottomEdgeOfFace (faceX, faceY) = (faceY + 1) * 50 - 1
let leftEdgeOfFace (faceX, faceY) = faceX * 50
let rightEdgeOfFace (faceX, faceY) = (faceX + 1) * 50 - 1

let isAtTopEdgeOfFace face (x, y) =
    leftEdgeOfFace face <= x && x <= rightEdgeOfFace face && y = topEdgeOfFace face
let isAtBottomEdgeOfFace face (x, y) =
    leftEdgeOfFace face <= x && x <= rightEdgeOfFace face && y = bottomEdgeOfFace face
let isAtLeftEdgeOfFace face (x, y) =
    topEdgeOfFace face <= y && y <= bottomEdgeOfFace face && x = leftEdgeOfFace face
let isAtRightEdgeOfFace face (x, y) =
    topEdgeOfFace face <= y && y <= bottomEdgeOfFace face && x = rightEdgeOfFace face

let tryStep2 direction position =
    let (nextPosition, nextDirection) =
        match position, direction with
        | (x, y), Left when (x, y) |> isAtLeftEdgeOfFace (1, 0) ->
            ((leftEdgeOfFace (0, 2), bottomEdgeOfFace (0, 2) - offsetAtFace y), Right)
        | (x, y), Left when (x, y) |> isAtLeftEdgeOfFace (0, 2) ->
            ((leftEdgeOfFace (1, 0), bottomEdgeOfFace (1, 0) - offsetAtFace y), Right)

        | (x, y), Up when (x, y) |> isAtTopEdgeOfFace (1, 0) ->
            ((leftEdgeOfFace (0, 3), topEdgeOfFace (0, 3) + offsetAtFace x), Right)
        | (x, y), Left when (x, y) |> isAtLeftEdgeOfFace (0, 3) ->
            ((leftEdgeOfFace (1, 0) + offsetAtFace y, topEdgeOfFace (1, 0)), Down)

        | (x, y), Up when (x, y) |> isAtTopEdgeOfFace (2, 0) ->
            ((leftEdgeOfFace (0, 3) + offsetAtFace x, bottomEdgeOfFace (0, 3)), Up)
        | (x, y), Down when (x, y) |> isAtBottomEdgeOfFace (0, 3) ->
            ((leftEdgeOfFace (2, 0) + offsetAtFace x, topEdgeOfFace (2, 0)), Down)

        | (x, y), Right when (x, y) |> isAtRightEdgeOfFace (2, 0) ->
            ((rightEdgeOfFace (1, 2), bottomEdgeOfFace (1, 2) - offsetAtFace y), Left)
        | (x, y), Right when (x, y) |> isAtRightEdgeOfFace (1, 2) ->
            ((rightEdgeOfFace (2, 0), bottomEdgeOfFace (2, 0) - offsetAtFace y), Left)

        | (x, y), Down when (x, y) |> isAtBottomEdgeOfFace (2, 0) ->
            ((rightEdgeOfFace (1, 1), topEdgeOfFace (1, 1) + offsetAtFace x), Left)
        | (x, y), Right when (x, y) |> isAtRightEdgeOfFace (1, 1) ->
            ((leftEdgeOfFace (2, 0) + offsetAtFace y, bottomEdgeOfFace (2, 0)), Up)

        | (x, y), Down when (x, y) |> isAtBottomEdgeOfFace (1, 2) ->
            ((rightEdgeOfFace (0, 3), topEdgeOfFace (0, 3) + offsetAtFace x), Left)
        | (x, y), Right when (x, y) |> isAtRightEdgeOfFace (0, 3) ->
            ((leftEdgeOfFace (1, 2) + offsetAtFace y, bottomEdgeOfFace (1, 2)), Up)

        | (x, y), Left when (x, y) |> isAtLeftEdgeOfFace (1, 1) ->
            ((leftEdgeOfFace (0, 2) + offsetAtFace y, topEdgeOfFace (0, 2)), Down)
        | (x, y), Up when (x, y) |> isAtTopEdgeOfFace (0, 2) ->
            ((leftEdgeOfFace (1, 1), topEdgeOfFace (1, 1) + offsetAtFace x), Right)

        | (x, y), Left -> ((x - 1, y), Left)
        | (x, y), Right -> ((x + 1, y), Right)
        | (x, y), Up -> ((x, y - 1), Up)
        | (x, y), Down -> ((x, y + 1), Down)

    match getTile nextPosition with
    | Void -> failwith $"Moved from %A{position} to %A{nextPosition} which is void"
    | Open -> Some (nextPosition, nextDirection)
    | Wall -> None

let printState ((x, y), direction) =
    [
        for y' in [Math.Max(0, y - 10) .. Math.Min(mapHeight - 1, y + 10)] do
            [
                for x' in [Math.Max(0, x - 10) .. Math.Min(mapWidth - 1, x + 10)] do
                    if (x', y') = (x, y) then
                        match direction with
                        | Up -> "^"
                        | Down -> "v"
                        | Left -> "<"
                        | Right -> ">"
                    else
                        match map.[y'].[x'] with
                        | Void -> " "
                        | Open -> "."
                        | Wall -> "#"
            ]
            |> String.concat ""
    ]
    |> String.concat Environment.NewLine
    |> printfn "%A\r\n%s" (x, y)

let rec walk2 ((x, y), direction) instruction =
    match instruction with
    | ChangeDirection (TurnLeft as turn)
    | ChangeDirection (TurnRight as turn) -> ((x, y), Direction.change turn direction)
    | Go 0 -> ((x, y), direction)
    | Go steps ->
        match tryStep2 direction (x, y) with
        | Some state -> walk2 state (Go (steps - 1))
        | None -> ((x, y), direction)

(((startX, 0), Right), instructions)
||> List.fold walk2
|> getResult
|> printfn "Part 2: %d"

// let testStep position direction expected =
//     let actual = tryStep2 direction position
//     printfn "%A %A -> %A - %b" position direction actual (actual = Some expected)

// testStep (50, 0) Left ((0, 149), Right)
// testStep (0, 149) Left ((50, 0), Right)
// testStep (50, 49) Left ((0, 100), Right)
// testStep (0, 100) Left ((50, 49), Right)

// testStep (50, 0) Up ((0, 150), Right)
// testStep (0, 150) Left ((50, 0), Down)
// testStep (99, 0) Up ((0, 199), Right)
// testStep (0, 199) Left ((99, 0), Down)

// testStep (100, 0) Up ((0, 199), Up)
// testStep (0, 199) Down ((100, 0), Down)
// testStep (149, 0) Up ((49, 199), Up)
// testStep (49, 199) Down ((149, 0), Down)

// testStep (149, 0) Right ((99, 149), Left)
// testStep (99, 149) Right ((149, 0), Left)
// testStep (149, 49) Right ((99, 100), Left)
// testStep (99, 100) Right ((149, 49), Left)

// testStep (148, 49) Down ((99, 98), Left)
// testStep (99, 98) Right ((148, 49), Up)
// testStep (101, 49) Down ((99, 51), Left)
// testStep (99, 51) Right ((101, 49), Up)

// testStep (99, 149) Down ((49, 199), Left)
// testStep (49, 199) Right ((99, 149), Up)
// testStep (50, 149) Down ((49, 150), Left)
// testStep (49, 150) Right ((50, 149), Up)

// testStep (50, 50) Left ((0, 100), Down)
// testStep (0, 100) Up ((50, 50), Right)
// testStep (50, 99) Left ((49, 100), Down)
// testStep (49, 100) Up ((50, 99), Right)
