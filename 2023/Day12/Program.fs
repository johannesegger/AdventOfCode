open Checked
open System.IO

type SpringState = Operational | Damaged | Unknown

let parseSpringState = function
    | '.' -> Operational
    | '#' -> Damaged
    | '?' -> Unknown
    | v -> failwith $"Invalid spring state: %c{v}" 

let parseRecord (line: string) =
    let parts = line.Split(' ')
    let conditionRecord = parts.[0] |> Seq.map parseSpringState |> Seq.toList
    let damagedCounts = parts.[1].Split(',') |> Seq.map int |> Seq.toList
    (conditionRecord, damagedCounts)

let getArrangements (record, counts) =
    let mutable cache = Map.empty
    let rec fn record counts currentDamagedCount : int64 =
        match Map.tryFind (record, counts, currentDamagedCount) cache with
        | Some v -> v
        | None ->
            let count: int64 =
                match counts, record with
                | [], _ when List.forall (fun record -> record = Operational || record = Unknown) record -> 1
                | [], _ -> 0
                | [ count ], [] when count = currentDamagedCount -> 1
                | _, [] -> 0
                | count :: otherCounts, Damaged :: otherRecord ->
                    if currentDamagedCount < count then
                        fn otherRecord (count :: otherCounts) (currentDamagedCount + 1)
                    else 0
                | count :: otherCounts, Unknown :: otherRecord ->
                    if currentDamagedCount = 0 then // try damaged and operational
                        let countWhenDamaged = fn otherRecord (count :: otherCounts) 1
                        let countWhenOperational = fn otherRecord (count :: otherCounts) 0
                        countWhenDamaged + countWhenOperational
                    elif currentDamagedCount < count then // only damaged would be valid
                        fn otherRecord (count :: otherCounts) (currentDamagedCount + 1)
                    elif currentDamagedCount = count then // only operational would be valid
                        fn otherRecord otherCounts 0
                    else 0 // Shouldn't happen
                | count :: otherCounts, Operational :: otherRecord ->
                    if currentDamagedCount = 0 then // yet another '.'
                        fn otherRecord (count :: otherCounts) 0
                    elif currentDamagedCount = count then // first '.' after correct number of damaged
                        fn otherRecord otherCounts 0
                    else 0
            cache <- Map.add (record, counts, currentDamagedCount) count cache
            count

    fn record counts 0

File.ReadAllLines "input.txt"
|> Seq.map parseRecord
|> Seq.sumBy getArrangements
|> printfn "Part 1: %d"

let unfoldRecord (record, counts) =
    (
        [ for i in 1..5 do
            if i > 1 then Unknown
            yield! record
        ],
        [ for _ in 1..5 do yield! counts ]
    )

File.ReadAllLines "input.txt"
|> Seq.map (parseRecord >> unfoldRecord)
|> Seq.sumBy getArrangements
|> printfn "Part 2: %d"
