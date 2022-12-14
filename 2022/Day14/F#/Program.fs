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

let rec findRestPosition (x, y) map =
    if Set.exists (fun (x', y') -> y' > y) map then
        if Set.contains (x, y + 1) map then
            if Set.contains (x - 1, y + 1) map then
                if Set.contains (x + 1, y + 1) map then Some (x, y)
                else findRestPosition (x + 1, y + 1) map
            else findRestPosition (x - 1, y + 1) map
        else findRestPosition (x, y + 1) map
    else None

let rec fillWithSand sandUnits map =
    match findRestPosition (500, 0) map with
    | Some c ->
        let map' = Set.add c map
        fillWithSand (sandUnits + 1)  map'
    | None -> sandUnits

File.ReadLines("input.txt")
|> Seq.fold parseRockLines Set.empty
|> fillWithSand 0
|> printfn "%A"
