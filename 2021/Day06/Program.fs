open Checked
open System.IO

let parseDuration (text: string) = text.Split(',') |> Seq.map int |> Seq.toList

let rec runNaive days durations =
    if days = 0 then List.length durations
    else
        let newFishCount = durations |> List.filter (fun v -> v = 0) |> List.length
        let newFish = List.replicate newFishCount 8
        let durations' = durations |> List.map (fun v -> if v = 0 then 6 else v - 1)
        runNaive (days - 1) (durations' @ newFish)

let groupDurations durations =
    [0..8]
    |> List.map (fun duration -> (duration, durations |> List.filter ((=) duration) |> List.length |> int64))
    |> Map.ofList

let rec run days durations =
    if days = 0 then durations |> Map.values |> Seq.sum
    else
        let newFishCount = Map.find 0 durations
        let durations' =
            durations
            |> Map.map (fun duration _count ->
                if duration = 8 then Map.find 0 durations
                elif duration = 6 then Map.find (duration + 1) durations + newFishCount
                else Map.find (duration + 1) durations
            )
        run (days - 1) durations'

File.ReadAllLines "input.txt"
|> Seq.exactlyOne
|> parseDuration
|> groupDurations
|> run 80
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.exactlyOne
|> parseDuration
|> groupDurations
|> run 256
|> printfn "Part 2: %d"
