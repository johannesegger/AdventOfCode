open Checked
open System.IO

type MapItem = Wall | Box

let parseMapItem v =
    if v = '#' then Some Wall
    elif v = 'O' || v = '[' then Some Box
    else None

let parseMap (lines: string[]) =
    let map = [
        for row in 0..lines.Length - 1 do
        for col in 0..lines.[row].Length - 1 do
            match parseMapItem lines.[row].[col] with
            | Some v -> ((row, col), v)
            | None -> ()
    ]
    let position =
        [
            for row in 0..lines.Length - 1 do
            for col in 0..lines.[row].Length - 1 -> (row, col)
        ]
        |> List.find (fun (row, col) -> lines.[row].[col] = '@')
    (Map.ofList map, position)

type Move = Up | Down | Left | Right

let parseMove v =
    if v = '^' then Up
    elif v = 'v' then Down
    elif v = '<' then Left
    elif v = '>' then Right
    else failwith $"Invalid move: %c{v}"

let parse lines =
    let (map, position) = lines |> Array.takeWhile ((<>)"") |> parseMap
    let moves = lines |> Seq.skipWhile ((<>)"") |> Seq.skip 1 |> String.concat "" |> Seq.map parseMove |> Seq.toList
    (map, position, moves)

let step (row, col) = function
    | Up -> (row - 1, col)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Right -> (row, col + 1)

let draw map position =
    let minRow = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> row) |> Seq.min
    let maxRow = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> row) |> Seq.max
    let minCol = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> col) |> Seq.min
    let maxCol = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> col) |> Seq.max
    [
        for row in minRow..maxRow do
        [
            for col in minCol..maxCol do
                if (row, col) = position then "@"
                else
                    match Map.tryFind (row, col) map with
                    | Some Box -> "O"
                    | Some Wall -> "#"
                    | None -> "."
        ]
        |> String.concat ""
    ]
    |> String.concat "\n"
    |> printfn "%s\n"

let getBoxesEnd map position direction =
    let rec fn (row, col) =
        let (row', col') = step (row, col) direction
        match Map.tryFind (row', col') map with
        | None -> Some (row', col')
        | Some Wall -> None
        | Some Box -> fn (row', col')

    fn position

let move (map, position) direction =
    match getBoxesEnd map position direction with
    | Some lastPosition ->
        let position' = step position direction
        let map' =
            if lastPosition = position' then map
            else
                map
                |> Map.remove position'
                |> Map.add lastPosition map.[position']
        (map', position')
    | None -> (map, position)

let run (map, position, moves) =
    moves
    |> List.fold move (map, position)

let getGPSCoordinates (map, position) =
    map
    |> Map.toSeq
    |> Seq.choose (fun (position, mapItem) -> match mapItem with Box -> Some position | Wall -> None)
    |> Seq.sumBy (fun (row, col) -> 100 * row + col)

File.ReadAllLines "input.txt"
|> parse
|> run
|> getGPSCoordinates
|> printfn "Part 1: %d"

let parseMap' (lines: string[]) =
    lines
    |> Array.map (fun line ->
        line.Replace("#", "##")
            .Replace("O", "[]")
            .Replace(".", "..")
            .Replace("@", "@.")
    )
    |> parseMap

let parse' lines =
    let (map, position) = lines |> Array.takeWhile ((<>)"") |> parseMap'
    let moves = lines |> Seq.skipWhile ((<>)"") |> Seq.skip 1 |> String.concat "" |> Seq.map parseMove |> Seq.toList
    (map, position, moves)

let draw' map position =
    let minRow = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> row) |> Seq.min
    let maxRow = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> row) |> Seq.max
    let minCol = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> col) |> Seq.min
    let maxCol = map |> Map.toSeq |> Seq.map (fun ((row, col), _) -> col) |> Seq.max
    [
        for row in minRow..maxRow do
        [
            for col in minCol..maxCol do
                if (row, col) = position then "@"
                else
                    if Map.tryFind (row, col - 1) map <> Some Box then
                        match Map.tryFind (row, col) map with
                        | Some Box -> "[]"
                        | Some Wall -> "#"
                        | None -> "."
        ]
        |> String.concat ""
    ]
    |> String.concat "\n"
    |> printfn "%s\n"

let tryFindBoxToMove map position =
    [ position; step position Left ]
    |> List.tryFind (fun p -> Map.tryFind p map = Some Box)
    |> function
    | Some boxPosition -> (boxPosition, Some Box)
    | None -> (position, Map.tryFind position map)

let getBoxesToMove map position direction =
    let rec fn position acc =
        match tryFindBoxToMove map (step position direction) with
        | _, None -> Some acc
        | _, Some Wall -> None
        | position', Some Box ->
            let acc' = position' :: acc
            match direction with
            | Up | Down ->
                fn position' acc'
                |> Option.bind (fn (step position' Right))
            | Left -> fn position' acc'
            | Right -> fn (step position' Right) acc'

    fn position [] |> Option.map List.distinct

let move' (map, position) direction =
    match getBoxesToMove map position direction with
    | Some positions ->
        let map' =
            positions
            |> List.fold (fun map position ->
                map
                |> Map.remove position
                |> Map.add (step position direction) map.[position]
            ) map
            |> Map.remove (step position direction)
        let position' = step position direction
        (map', position')
    | None -> (map, position)

let run' (map, position, moves) =
    moves
    |> List.fold move' (map, position)

File.ReadAllLines "input.txt"
|> parse'
|> run'
|> getGPSCoordinates
|> printfn "Part 2: %d"
