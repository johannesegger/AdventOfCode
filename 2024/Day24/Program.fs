open Checked
open System.IO
open System

let parseWire (line: string) =
    let parts = line.Split(':', StringSplitOptions.TrimEntries)
    (parts.[0], parts.[1] = "1")

type Operation = And | Or | XOr
module Operation =
    let parse = function
        | "AND" -> And
        | "OR" -> Or
        | "XOR" -> XOr
        | v -> failwith $"Invalid gate type %s{v}"
    let run (a, b) = function
        | And -> a && b
        | Or -> a || b
        | XOr -> a <> b // https://stackoverflow.com/a/7656064/1293659

type Gate = {
    In: Set<string>
    Op: Operation
    Out: string
}
module Gate =
    let parse (line: string) =
        let parts = line.Split()
        {
            In = Set.ofList [parts.[0]; parts.[2]]
            Op = Operation.parse parts.[1]
            Out = parts.[4]
        }

let parse lines =
    let outputs =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.map parseWire
        |> Seq.fold (fun map (wire, value) -> Map.add wire value map) Map.empty
    let gates = lines |> Seq.skipWhile ((<>) "") |> Seq.skip 1 |> Seq.map Gate.parse |> Seq.toList
    (outputs, gates)

let tryStoreOutput map gate =
    let wires =
        gate.In
        |> Seq.choose (fun wire -> Map.tryFind wire map)
        |> Seq.toList

    match wires with
    | [wireIn1; wireIn2] -> Map.add gate.Out (gate.Op |> Operation.run (wireIn1, wireIn2)) map
    | _ -> map

let getFinalOutput (map, gates) =
    let rec fn map =
        let map' = gates |> Seq.fold tryStoreOutput map
        if Map.count map' = Map.count map then map
        else fn map'
    fn map
    |> Map.toSeq
    |> Seq.filter (fun (wire: string, _value) -> wire.StartsWith "z")
    |> Seq.sortByDescending (fun (wire, _value) -> int wire.[1..])
    |> Seq.map (fun (_wire, value) -> if value then 1 else 0)
    |> Seq.fold (fun n d -> n * 2L + int64 d) 0L

File.ReadAllLines "input.txt"
|> parse
|> getFinalOutput
|> printfn "Part 1: %d"

let requireGate wireIn1 operation wireIn2 gates =
    let wires = Set.ofList [wireIn1; wireIn2]
    // Check if gate exists
    match gates |> List.tryPick (fun g -> if g.In = wires && g.Op = operation then Some g.Out else None) with
    | Some result -> (result, Set.empty)
    | None ->
        // Check if gate 'wire1 op ...' exists
        match gates |> List.tryPick (fun g -> if Set.intersect g.In wires |> Set.isEmpty |> not && g.Op = operation then Some g else None) with
        | Some gate ->
            let missingWire = Set.difference wires gate.In |> Seq.exactlyOne
            let foreignWire = Set.difference gate.In wires |> Seq.exactlyOne
            (gate.Out, Set.ofList [missingWire; foreignWire])
        | None ->
            failwith "requireGate failed - don't know what to swap"

let getGatesToSwap gates =
    let rec fn i carry gates acc =
        // x00 XOR y00 -> z00 - result digit
        // x00 AND y00 -> rvh - carry

        // y01 XOR x01 -> kgc - source digit sum
        // rvh XOR kgc -> z01 - result digit
        // rvh AND kgc -> vtk - carry option 1
        // x01 AND y01 -> rcm - carry option 2
        // vtk OR rcm -> fbc - carry
        let sourceDigitSumWire = gates |> List.tryPick (fun g -> if g.In = Set.ofList [$"x%02d{i}"; $"y%02d{i}"] && g.Op = XOr then Some g.Out else None)
        match sourceDigitSumWire with
        | None -> acc
        | Some sourceDigitSumWire ->
            let (carry1, swappedWires) = gates |> requireGate carry And sourceDigitSumWire
            let acc = Set.union swappedWires acc
            let (carry2, swappedWires) = gates |> requireGate $"x%02d{i}" And $"y%02d{i}"
            let acc = Set.union swappedWires acc
            let (carry, swappedWires) = gates |> requireGate carry1 Or carry2
            let acc = Set.union swappedWires acc
            fn (i + 1) carry gates acc
    let carry = gates |> List.pick (fun g -> if g.In = Set.ofList ["x00"; "y00"] && g.Op = And then Some g.Out else None)
    fn 1 carry gates Set.empty

File.ReadAllLines "input.txt"
|> parse
|> snd
|> getGatesToSwap
|> String.concat ","
|> printfn "Part 2: %s"
