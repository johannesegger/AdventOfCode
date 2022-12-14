open System
open System.IO

let parseCoordinates (v: string) =
    v.Split(',')
    |> Array.map int
    |> fun v -> v.[0], v.[1]

let getVertexCoordinates (x1: int, y1: int) (x2: int, y2: int) =
    if x1 = x2 then
        let l = if y2 > y1 then [y1..y2] else [y2..y1]
        [ for y in l -> (x1, y) ] 
    elif y1 = y2 then
        let l = if x2 > x1 then [x1..x2] else [x2..x1]
        [ for x in l -> (x, y1) ] 
    else
        failwith $"Can't get vertex coordinates between %A{(x1, y1)} and %A{(x2, y2)}"

let getRockCoordinates (line: string) =
    line.Split("->", StringSplitOptions.TrimEntries)
    |> Array.map parseCoordinates
    |> Seq.pairwise
    |> Seq.collect (fun (a, b) -> getVertexCoordinates a b)
    |> fun c -> printfn "%A" c; c

let addRockCoordinates state c =
    Set.add c state

let parseRockLines state line =
    getRockCoordinates line
    |> Seq.fold addRockCoordinates state

type SandUnitAction = MoveTo of int * int | Drop | Rest

let rec findRestPosition coords getAction map =
    match getAction coords map with
    | Drop -> None
    | MoveTo (x, y) -> findRestPosition (x, y) getAction map
    | Rest -> Some coords

let rec fillWithSand sandUnits getAction map =
    match findRestPosition (500, 0) getAction map with
    | Some c ->
        let map' = Set.add c map
        fillWithSand (sandUnits + 1) getAction map'
    | None -> sandUnits

let map =
    File.ReadLines("input.txt")
    |> Seq.fold parseRockLines Set.empty

let maxY = map |> Set.toSeq |> Seq.map snd |> Seq.max
let getAction (x, y) map =
    if y = maxY then Drop
    elif not <| Set.contains (x, y + 1) map then MoveTo (x, y + 1)
    elif not <| Set.contains (x - 1, y + 1) map then MoveTo (x - 1, y + 1)
    elif not <| Set.contains (x + 1, y + 1) map then MoveTo (x + 1, y + 1)
    else Rest

map
|> fillWithSand 0 getAction
|> printfn "Part 1: %d"

let floor = maxY + 2
let getAction2 (x, y) map =
    if Set.contains (500, 0) map then Drop
    elif y = floor - 1 then Rest
    elif not <| Set.contains (x, y + 1) map then MoveTo (x, y + 1)
    elif not <| Set.contains (x - 1, y + 1) map then MoveTo (x - 1, y + 1)
    elif not <| Set.contains (x + 1, y + 1) map then MoveTo (x + 1, y + 1)
    else Rest

map
|> fillWithSand 0 getAction2
|> printfn "Part 1: %d"
