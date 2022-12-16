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

let getNextStates remainingMinutes ((currentValves, openedValves), releasedPressure) =
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

    let getPressure valve =
        let flowRate = Map.find valve map |> fst
        (remainingMinutes - 1) * flowRate

    currentValves
    |> List.map getActions
    |> allPairs
    |> List.map (fun actions ->
        let nextValves = actions |> List.map fst
        let additionalOpenedValves = actions |> List.map snd |> Set.unionMany
        let openedValves' = Set.union openedValves additionalOpenedValves
        let releasedPressure' =
            let add = additionalOpenedValves |> Seq.sumBy getPressure
            releasedPressure + add
        ((nextValves, openedValves'), releasedPressure')
    )

// TODO this is bad as we just guess the number of solutions to keep for the next round
let cutStates states =
    states
    |> List.sortByDescending (fun (_, releasedPressure) -> releasedPressure)
    |> fun x -> List.truncate 10_000 x

let rec run remainingMinutes states =
    if remainingMinutes = 0 then
        states
        |> List.map (fun (_, releasedPressure) -> releasedPressure)
        |> List.max
    else
        states
        |> List.collect (getNextStates remainingMinutes)
        |> cutStates
        |> run (remainingMinutes - 1)

let openedValves =
    map
    |> Map.toSeq
    |> Seq.choose (fun (valve, (flowRate, _)) ->
        if flowRate = 0 then Some valve
        else None
    )
    |> Set.ofSeq

run 30 [ ((["AA"], openedValves), 0) ]
|> printfn "Part 1: %d"

run 26 [ ((["AA"; "AA"], openedValves), 0) ]
|> printfn "Part 2: %d"
