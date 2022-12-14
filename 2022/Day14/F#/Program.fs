open System
open System.IO

let parseCoordinates (v: string) =
    let parts = v.Split(',')
    int parts.[0], int parts.[1]

let getVertexCoordinates (x1, y1) (x2, y2) =
    if x1 <> x2 && y1 <> y2 then
        failwith $"Can't get vertex coordinates between %A{(x1, y1)} and %A{(x2, y2)}"
    [
        for x in [x1 .. x2] -> (x, y1)
        for x in [x1 .. -1 .. x2] -> (x, y1)
        for y in [y1 ..y2] -> (x1, y)
        for y in [y1 .. -1 .. y2] -> (x1, y)
    ]

let getRockCoordinates (line: string) =
    line.Split("->", StringSplitOptions.TrimEntries)
    |> Array.map parseCoordinates
    |> Seq.pairwise
    |> Seq.collect (fun (a, b) -> getVertexCoordinates a b)

let addRockCoordinates state c =
    Set.add c state

let parseRockLines state line =
    getRockCoordinates line
    |> Seq.fold addRockCoordinates state

type SandUnitAction = Drop | Rest | MoveTo of int * int

let rec findRestPosition coords getAction map =
    match getAction coords map with
    | Drop -> None
    | Rest -> Some coords
    | MoveTo (x, y) -> findRestPosition (x, y) getAction map

let sandStartPosition = (500, 0)

let rec fillWithSand sandUnits getAction map =
    match findRestPosition sandStartPosition getAction map with
    | Some c ->
        let map' = Set.add c map
        fillWithSand (sandUnits + 1) getAction map'
    | None -> sandUnits

let map =
    File.ReadLines("input.txt")
    |> Seq.fold parseRockLines Set.empty

let maxY = map |> Set.toSeq |> Seq.map snd |> Seq.max

map
|> fillWithSand 0 (fun (x, y) map ->
    if y = maxY then Drop
    elif not <| Set.contains (x, y + 1) map then MoveTo (x, y + 1)
    elif not <| Set.contains (x - 1, y + 1) map then MoveTo (x - 1, y + 1)
    elif not <| Set.contains (x + 1, y + 1) map then MoveTo (x + 1, y + 1)
    else Rest
)
|> printfn "Part 1: %d"

let floor = maxY + 2
map
|> fillWithSand 0 (fun (x, y) map ->
    if Set.contains sandStartPosition map then Drop
    elif y = floor - 1 then Rest
    elif not <| Set.contains (x, y + 1) map then MoveTo (x, y + 1)
    elif not <| Set.contains (x - 1, y + 1) map then MoveTo (x - 1, y + 1)
    elif not <| Set.contains (x + 1, y + 1) map then MoveTo (x + 1, y + 1)
    else Rest
)
|> printfn "Part 2: %d"
