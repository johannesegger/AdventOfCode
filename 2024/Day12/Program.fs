open Checked
open System.IO

let getPositions (map: string[]) =
    [
        for row in 0..map.Length - 1 do
        for col in 0..map.[row].Length - 1 do
            (row, col)
    ]

let getAllNeighbors (row, col) =
    Set.ofList [
        (row - 1, col)
        (row, col + 1)
        (row + 1, col)
        (row, col - 1)
    ]

let getRegionNeighbors (map: string[]) (row, col) =
    getAllNeighbors (row, col)
    |> Set.filter (fun (r, c) -> map |> Seq.tryItem r |> Option.bind (Seq.tryItem c) |> Option.isSome)
    |> Set.filter (fun (r, c) -> map.[r].[c] = map.[row].[col])

let getRegion map p =
    let rec fn p acc =
        let neighbors = Set.difference (getRegionNeighbors map p) acc
        let acc' = Set.union acc neighbors
        neighbors
        |> Seq.fold (fun acc'' p -> fn p acc'') acc'
    fn p (Set.singleton p)

let getArea = Set.count
let getPerimeter region =
    region
    |> Seq.sumBy (fun p ->
        Set.difference (getAllNeighbors p) region |> Set.count
    )

let getRegions map =
    let rec fn positions acc =
        match positions with
        | [] -> acc
        | p :: ps ->
            let region = getRegion map p
            let ps' = ps |> List.except region
            fn ps' (region :: acc)
    fn (getPositions map) []

File.ReadAllLines "input.txt"
|> getRegions
|> List.sumBy (fun region -> getArea region * getPerimeter region)
|> printfn "Part 1: %d"

let getSides region =
    let (startRow, startCol) = (region |> Seq.map fst |> Seq.min, region |> Seq.map snd |> Seq.min)
    let (endRow, endCol) = (region |> Seq.map fst |> Seq.max, region |> Seq.map snd |> Seq.max)
    List.sum [
        for row in startRow..endRow do
        for col in startCol..endCol do
            if Set.contains (row, col) region then
                // start new horizontal top line
                if not (Set.contains (row - 1, col) region) then
                    if not (Set.contains (row, col - 1) region) || Set.contains (row - 1, col - 1) region then 1
                // start new horizontal bottom line
                if not (Set.contains (row + 1, col) region) then
                    if not (Set.contains (row, col - 1) region) || Set.contains (row + 1, col - 1) region then 1
                // start new vertical left line
                if not (Set.contains (row, col - 1) region) then
                    if not (Set.contains (row - 1, col) region) || Set.contains (row - 1, col - 1) region then 1
                // start new vertical right line
                if not (Set.contains (row, col + 1) region) then
                    if not (Set.contains (row - 1, col) region) || Set.contains (row - 1, col + 1) region then 1

    ]

File.ReadAllLines "input.txt"
|> getRegions
|> List.sumBy (fun region -> getArea region * getSides region)
|> printfn "Part 2: %d"
