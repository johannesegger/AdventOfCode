open System
open System.IO
open System.Text.RegularExpressions

let parseLine v =
    let m = Regex.Match(v, @"^Valve (\w+) has flow rate=(\d+); tunnel(?:s?) lead(?:s?) to valve(?:s?) (.*)$")
    if not m.Success then failwith $"Can't parse line \"%s{v}\""
    let name = m.Groups.[1].Value
    let flowRate = int m.Groups.[2].Value
    let targets = m.Groups.[3].Value.Split(',', StringSplitOptions.TrimEntries)
    (name, (flowRate, Array.toList targets))

let map =
    File.ReadLines("input.txt")
    |> Seq.map parseLine
    |> Map.ofSeq

let allPairs lists =
    let rec fn lists acc =
        match lists with
        | [] -> acc
        | list :: lists ->
            let acc' =
                [
                    for v in list do
                    for result in acc -> v :: result
                ]
            fn lists acc'
    fn lists [[]]

let getActions openedValves valve =
    [
        if not <| Set.contains valve openedValves then
            (valve, Set.singleton valve)
        yield!
            Map.find valve map |> snd
            |> List.map (fun valve ->
                (valve, Set.empty)
            )
    ]

let getPressure remainingMinutes valve =
    let flowRate = Map.find valve map |> fst
    (remainingMinutes - 1) * flowRate

let getNextStates remainingMinutes (releasedPressure, currentValves, openedValves) =
    currentValves
    |> List.map (getActions openedValves)
    |> allPairs
    |> List.map (fun actions ->
        let nextValves =
            actions
            |> List.map fst
        let additionalOpenedValves =
            actions
            |> List.map snd
            |> Set.unionMany
        let additionalReleasedPressure =
            additionalOpenedValves
            |> Seq.sumBy (getPressure remainingMinutes)
        (releasedPressure + additionalReleasedPressure, nextValves, Set.union openedValves additionalOpenedValves)
    )

let rec run remainingMinutes states =
    if remainingMinutes = 0 then
        states
        |> List.map (fun (releasedPressure, _, _) -> releasedPressure)
        |> List.max
    else
        states
        |> List.collect (getNextStates remainingMinutes)
        |> List.sortByDescending (fun (releasedPressure, _, _) -> releasedPressure)
        |> fun x -> List.truncate 1000 x
        |> run (remainingMinutes - 1)

let openedValves =
    map
    |> Map.toSeq
    |> Seq.choose (fun (valve, (flowRate, _)) ->
        if flowRate = 0 then Some valve
        else None
    )
    |> Set.ofSeq

run 30 [ (0, ["AA"], openedValves) ]
|> printfn "Part 1: %d"

run 26 [ (0, ["AA"; "AA"], openedValves) ]
|> printfn "Part 2: %d"
