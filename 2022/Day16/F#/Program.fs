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

let getNextStates1 remainingMinutes (releasedPressure, currentValve, openedValves) =
    [
        if not <| Set.contains currentValve openedValves then
            let openedValves' = Set.add currentValve openedValves
            let flowRate = Map.find currentValve map |> fst
            let releasedPressure' = releasedPressure + (remainingMinutes - 1) * flowRate
            (releasedPressure', currentValve, openedValves')
        yield!
            Map.find currentValve map |> snd
            |> List.map (fun valve ->
                (releasedPressure, valve, openedValves)
            )
    ]

let getNextStates2 remainingMinutes (releasedPressure, (valve1, valve2), openedValves) =
    let getActions valve =
        [
            if not <| Set.contains valve openedValves then
                (valve, Set.singleton valve)
            yield!
                Map.find valve map |> snd
                |> List.map (fun valve ->
                    (valve, Set.empty)
                )
        ]
    List.allPairs (getActions valve1) (getActions valve2)
    |> List.map (fun ((valve1, openedValves1), (valve2, openedValves2)) ->
        let additionalOpenedValves = Set.union openedValves1 openedValves2
        let additionalReleasedPressure =
            additionalOpenedValves
            |> Seq.sumBy (fun valve ->
                let flowRate = Map.find valve map |> fst
                (remainingMinutes - 1) * flowRate
            )
        (releasedPressure + additionalReleasedPressure, (valve1, valve2), Set.union openedValves additionalOpenedValves)
    )

let rec run remainingMinutes getNextStates states =
    if remainingMinutes = 0 then
        states
        |> List.map (fun (releasedPressure, _, _) -> releasedPressure)
        |> List.max
    else
        states
        |> List.collect (getNextStates remainingMinutes)
        |> List.sortByDescending (fun (releasedPressure, _, _) -> releasedPressure)
        |> fun x -> List.truncate 1000 x
        |> run (remainingMinutes - 1) getNextStates

let openedValves =
    map
    |> Map.toSeq
    |> Seq.choose (fun (valve, (flowRate, _)) ->
        if flowRate = 0 then Some valve
        else None
    )
    |> Set.ofSeq

run 30 getNextStates1 [ (0, "AA", openedValves) ]
|> printfn "Part 1: %d"

run 26 getNextStates2 [ (0, ("AA", "AA"), openedValves) ]
|> printfn "Part 2: %d"
