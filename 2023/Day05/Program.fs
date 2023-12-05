open Checked
open System.IO

type SeedMap = {
    DestinationStart: int64
    SourceStart: int64
    Range: int64
}

let parseMap lines header =
    lines
    |> Seq.skipWhile ((<>) header)
    |> Seq.skip 1 
    |> Seq.takeWhile ((<>) "")
    |> Seq.map (fun v ->
        let parts = v.Split()
        {
            DestinationStart = int64 parts.[0]
            SourceStart = int64 parts.[1]
            Range = int64 parts.[2]
        }
    )
    |> Seq.sortBy _.SourceStart
    |> Seq.toList

let parse (lines: string array) =
    let seeds =
        lines.[0].Substring("seeds: ".Length).Split()
        |> Seq.map int64
        |> Seq.toList

    (seeds, [
        parseMap lines "seed-to-soil map:"
        parseMap lines "soil-to-fertilizer map:"
        parseMap lines "fertilizer-to-water map:"
        parseMap lines "water-to-light map:"
        parseMap lines "light-to-temperature map:"
        parseMap lines "temperature-to-humidity map:"
        parseMap lines "humidity-to-location map:"
    ])

let isInRange value entry =
    value >= entry.SourceStart && value < entry.SourceStart + entry.Range

let map value mapEntries =
    mapEntries
    |> List.tryPick (fun entry ->
        if isInRange value entry then Some (entry.DestinationStart + (value - entry.SourceStart))
        else None
    )
    |> Option.defaultValue value

let solveSingleSeeds (seeds, maps) =
    seeds
    |> List.map (fun seed ->
        maps |> List.fold map seed
    )
    |> List.min

File.ReadAllLines "input.txt"
|> parse
|> solveSingleSeeds
|> printfn "Part 1: %d"

let convertToSeedRanges (seeds, maps) =
    let seeds' =
        seeds
        |> List.chunkBySize 2
        |> List.map (fun v -> v.[0], v.[1])
    (seeds', maps)

let getDestinationRanges mapEntries (start, range) =
    let rec fn mapEntries value acc =
        if value >= start + range then acc
        else
            match mapEntries with
            | entry :: remainingMapEntries when entry.SourceStart > value ->
                let endValue = min (start + range) (entry.SourceStart + entry.Range)
                let acc' = (value, endValue - value) :: acc
                fn (entry :: remainingMapEntries) entry.SourceStart acc'
            | entry :: remainingMapEntries when isInRange value entry ->
                let endValue = min (start + range) (entry.SourceStart + entry.Range)
                let destinationStart = value - entry.SourceStart + entry.DestinationStart
                let destinationEnd = endValue - entry.SourceStart + entry.DestinationStart
                let acc' = (destinationStart, destinationEnd - destinationStart) :: acc
                fn remainingMapEntries endValue acc'
            | _ :: remainingMapEntries ->
                fn remainingMapEntries value acc
            | [] ->
                let acc' = (value, start + range - value) :: acc
                acc'

    fn mapEntries start []

let mapRange seedRanges mapEntries =
    seedRanges
    |> List.collect (getDestinationRanges mapEntries)

let solveRangeSeeds (seeds, maps) =
    maps
    |> List.fold mapRange seeds
    |> List.map fst
    |> List.min

File.ReadAllLines "input.txt"
|> parse
|> convertToSeedRanges
|> solveRangeSeeds
|> printfn "Part 2: %d"
