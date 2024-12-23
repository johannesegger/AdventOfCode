open Checked
open System.IO

let parseConnection (line: string) =
    let parts = line.Split('-')
    (parts.[0], parts.[1])

let addConnection map (a, b) =
    map
    |> Map.change a (fun vs -> vs |> Option.map (fun vs -> Set.add b vs) |> Option.defaultValue (Set.singleton b) |> Some)
    |> Map.change b (fun vs -> vs |> Option.map (fun vs -> Set.add a vs) |> Option.defaultValue (Set.singleton a) |> Some)

let parse lines =
    lines
    |> Seq.map parseConnection
    |> Seq.fold addConnection Map.empty

let findConnections connectionLength map =
    let computers =
        map
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.distinct
        |> Seq.toList
    let rec fn length connections =
        match connectionLength with
        | Some v when v <= length -> connections
        | _ ->
            let connections' =
                connections
                |> List.collect (fun list ->
                    computers
                    |> List.filter (fun computer -> Set.isSubset list map.[computer])
                    |> List.map (fun computer -> Set.add computer list)
                )
                |> List.distinct
            if List.isEmpty connections' then connections
            else fn (length + 1) connections'

    fn 1 (computers |> List.map Set.singleton)

File.ReadAllLines "input.txt"
|> parse
|> findConnections (Some 3)
|> List.filter (fun connection ->
    Set.count connection = 3 &&
    connection
    |> Seq.exists (fun computer -> computer.StartsWith 't')
)
|> List.length
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> parse
|> findConnections None
|> List.maxBy Set.count
|> String.concat ","
|> printfn "Part 2: %s"
