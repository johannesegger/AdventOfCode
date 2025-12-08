open Checked
open System.IO

type Location = {
    X: int
    Y: int
    Z: int
}

let parseLine (line: string) =
    let parts = line.Split(',')
    { X = int parts.[0]; Y = int parts.[1]; Z = int parts.[2] }

module List =
    let getDistinctPairs list =
        let rec fn list acc =
            match list with
            | x :: xs ->
                let pairs = xs |> List.map (fun y -> (x, y))
                fn xs (acc @ pairs)
            | [] -> acc

        fn list []

let getDistance (a, b) =
    let dx = b.X - a.X |> double
    let dy = b.Y - a.Y |> double
    let dz = b.Z - a.Z |> double
    System.Double.Sqrt(dx * dx + dy * dy + dz * dz)

let connectBoxes count locations =
    let rec fn circuits ((locationA, locationB), connections) =
        let circuitA = circuits |> List.find (Set.contains locationA)
        let circuitB = circuits |> List.find (Set.contains locationB)
        if circuitA = circuitB then
            if List.isEmpty connections then ((locationA, locationB), circuits)
            else fn circuits (List.head connections, List.tail connections)
        else
            let combinedCircuit = Set.union circuitA circuitB
            let circuits' =
                circuits
                |> List.except [circuitA; circuitB]
                |> List.append [combinedCircuit]
            if List.length circuits' = 1 || List.isEmpty connections then ((locationA, locationB), circuits')
            else fn circuits' (List.head connections, List.tail connections)

    let (nextConnection, connections) =
        locations
        |> List.getDistinctPairs
        |> List.sortBy getDistance
        |> fun pairs ->
            match count with
            | Some count -> List.take count pairs
            | None -> pairs
        |> function
        | x :: xs -> (x, xs)
        | [] -> failwith "No connections found."
    fn (locations |> List.map Set.singleton) (nextConnection, connections)

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.toList
|> connectBoxes (Some 1000)
|> snd
|> Seq.sortByDescending Set.count
|> Seq.take 3
|> Seq.map Set.count
|> Seq.reduce (*)
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.map parseLine
|> Seq.toList
|> connectBoxes None
|> fst
|> fun (a, b) -> int64 a.X * int64 b.X
|> printfn "Part 2: %d"
