open Checked
open System.IO

let parse (lines: string[]) =
    let splitters =
        [
            for row in 0..lines.Length - 1 do
            for col in 0..lines.[row].Length - 1 do
                if lines.[row].[col] = '^' then (row, col)
        ]
        |> Set.ofList
    let startPoint =
        [
            for row in 0..lines.Length - 1 do
            for col in 0..lines.[row].Length - 1 do
                if lines.[row].[col] = 'S' then (row, col)
        ]
        |> List.exactlyOne
    (startPoint, splitters)

let countSplits (startPoint, splitters) =
    let maxRow = splitters |> Seq.map fst |> Seq.max
    let rec fn row beamCols acc =
        if row > maxRow then acc
        else
            let splits =
                beamCols
                |> Seq.filter (fun beamCol ->
                    Set.contains (row, beamCol) splitters
                )
                |> Seq.length
            let beamCols' =
                beamCols
                |> Seq.collect (fun beamCol ->
                    if Set.contains (row, beamCol) splitters then [beamCol - 1; beamCol + 1]
                    else [beamCol]
                )
                |> Set.ofSeq
            fn (row + 1) beamCols' (acc + splits)
    fn 0 (Set.singleton (snd startPoint)) 0


File.ReadAllLines "input.txt"
|> parse
|> countSplits
|> printfn "Part 1: %d"

module Map =
    let addOrIncrease key count map =
        map
        |> Map.change key (function
            | Some v -> Some (v + count)
            | None -> Some count
        )

let countTimelines (startPoint, splitters) =
    let maxRow = splitters |> Seq.map fst |> Seq.max
    let rec fn row beamColToCount =
        if row > maxRow then
            beamColToCount |> Map.values |> Seq.sum
        else
            let beamColToCount' =
                (Map.empty, beamColToCount |> Map.toSeq)
                ||> Seq.fold (fun state (beamCol , count) ->
                    if Set.contains (row, beamCol) splitters then
                        state
                        |> Map.addOrIncrease (beamCol - 1) count
                        |> Map.addOrIncrease (beamCol + 1) count
                    else
                        state
                        |> Map.addOrIncrease beamCol count
                )
            fn (row + 1) beamColToCount'
    fn 0 (Map.ofList [(snd startPoint, 1L)])

File.ReadAllLines "input.txt"
|> parse
|> countTimelines
|> printfn "Part 2: %d"
