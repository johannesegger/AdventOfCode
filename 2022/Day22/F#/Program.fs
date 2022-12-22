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
let faceLength = mapWidth / 4

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
    map
    |> Array.tryItem y
    |> Option.bind (Array.tryItem x)
    |> Option.defaultValue Void

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
