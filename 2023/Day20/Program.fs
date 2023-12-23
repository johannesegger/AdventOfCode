open Checked
open System
open System.IO
open System.Text.RegularExpressions

type ModuleType = Broadcaster | FlipFlop of bool | Conjunction of Map<string, bool>

let parseConnectionLine (line: string) =
    let m = Regex.Match(line, @"^(%|&|)(\w+) -> (.+)$")
    (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value.Split(',', StringSplitOptions.TrimEntries) |> Array.toList)

let parseConnections (line: string array) =
    let entries =
        line
        |> Array.map parseConnectionLine
    entries
    |> Seq.map (fun (moduleType, moduleName, targets) ->
        if moduleType = "" then ("", (Broadcaster, targets))
        elif moduleType = "%" then (moduleName, (FlipFlop false, targets))
        else
            let inputs =
                entries
                |> Seq.filter (fun (_, _, targets) -> targets |> Seq.contains moduleName)
                |> Seq.map (fun (_, moduleName, _) -> (moduleName, false))
                |> Map.ofSeq
            (moduleName, (Conjunction inputs, targets))
    )
    |> Map.ofSeq

let processPulse connections (sender, pulse, receiver) =
    match Map.tryFind receiver connections with
    | Some (receiverType, nextReceivers) ->
        match receiverType with
        | Broadcaster -> ([ for nextReceiver in nextReceivers -> (receiver, pulse, nextReceiver) ], connections)
        | FlipFlop state ->
            if pulse then ([], connections)
            else
                let pulses = [ for nextReceiver in nextReceivers -> (receiver, not state, nextReceiver) ]
                let connections' = Map.add receiver (FlipFlop (not state), nextReceivers) connections
                (pulses, connections')
        | Conjunction inputs ->
            let inputs' = Map.add sender pulse inputs
            let pulses =
                if inputs' |> Map.forall (fun _ isHighPulse -> isHighPulse) then [ for nextReceiver in nextReceivers -> (receiver, false, nextReceiver) ]
                else [ for nextReceiver in nextReceivers -> (receiver, true, nextReceiver) ]
            let connections' = Map.add receiver (Conjunction inputs', nextReceivers) connections
            (pulses, connections')
    | None -> ([], connections)

let pushButton count connections =
    let rec fn count connections pulses (lowPulses, highPulses) =
        if count = 0 then (lowPulses * highPulses)
        elif List.isEmpty pulses then fn (count - 1) connections [ ("", false, "") ] (lowPulses, highPulses)
        else
            let (newHighPulses, newLowPulses) =
                pulses
                |> List.partition (fun (_, signal, _) -> signal)
                |> fun (a, b) -> (List.length a, List.length b)
            let (pulses', connections') =
                (connections, pulses)
                ||> List.mapFold processPulse
                |> fun (pulses, connections) -> (List.collect id pulses, connections)
            fn count connections' pulses' (lowPulses + newLowPulses, highPulses + newHighPulses)
    fn count connections [ ("", false, "") ] (0, 0)

File.ReadAllLines "input.txt"
|> parseConnections
|> pushButton 1000
|> printfn "Part 1: %A"

let pushButtonUntilModuleSendsPulse requiredPulse connections =
    let rec fn buttonPressCount connections pulses =
        if pulses |> List.exists (fun (sender, pulse, _) -> (sender, pulse) = requiredPulse) then buttonPressCount
        elif List.isEmpty pulses then
            fn (buttonPressCount + 1) connections [ ("", false, "") ]
        else
            let (pulses', connections') =
                (connections, pulses)
                ||> List.mapFold processPulse
                |> fun (pulses, connections) -> (List.collect id pulses, connections)
            fn buttonPressCount connections' pulses'
    fn 1 connections [ ("", false, "") ]

let pushButtonUntilRxLow connections =
    // &(fh, fz, mf, ss) -> &ql -> rx
    let moduleConnectedWithRx =
        connections
        |> Map.findKey (fun _ (_, targets) -> targets |> List.contains "rx")
    let dependencies =
        connections
        |> Map.filter (fun _ (_, targets) -> targets |> List.contains moduleConnectedWithRx)
        |> Map.keys
    dependencies
    |> Seq.map (fun moduleName -> pushButtonUntilModuleSendsPulse (moduleName, true) connections |> int64)
    |> Seq.reduce (*) // all numbers are prime numbers, so lcm is all numbers multiplied

File.ReadAllLines "input.txt"
|> parseConnections
|> pushButtonUntilRxLow
|> printfn "Part 2: %d"
