open Checked
open System.IO

let parseRange (text: string) =
    let parts = text.Split('-')
    int64 parts[0], int64 parts[1]

let parse lines =
    let ranges =
        lines
        |> Seq.takeWhile ((<>)"")
        |> Seq.map parseRange
        |> Seq.toList
    let ids =
        lines
        |> Seq.skipWhile ((<>)"")
        |> Seq.skip 1
        |> Seq.map int64
        |> Seq.toList
    ranges, ids

let isInAnyRange ranges ingredientId =
    ranges
    |> Seq.exists (fun (a, b) -> ingredientId >= a && ingredientId <= b)

let countValidIds (ranges, ids) =
    ids
    |> Seq.filter (isInAnyRange ranges)
    |> Seq.length

File.ReadAllLines "input.txt"
|> parse
|> countValidIds
|> printfn "Part 1: %d"

// r1 is fully included in r2
let isFullyIncluded (r1a, r1b) (r2a, r2b) =
    r1a >= r2a && r1a <= r2b && r1b >= r2a && r1b <= r2b

// r1 overlaps r2 at the left side
let overlapsLeft (r1a, r1b) (r2a, r2b) =
    r1a <= r2a && (r1b >= r2a && r1b <= r2b)

// r1 overlaps r2 at the right side
let overlapsRight (r1a, r1b) (r2a, r2b) =
    (r1a >= r2a && r1a <= r2b) && r1b >= r2b

// Remove portions of r1 so that including r2 doesn't include duplicates
let removeRange r1 r2 =
    if isFullyIncluded r1 r2 then []
    elif isFullyIncluded r2 r1 then [ (fst r1, fst r2 - 1L); (snd r2 + 1L, snd r1) ]
    elif overlapsLeft r1 r2 then [ (fst r1, fst r2 - 1L) ]
    elif overlapsRight r1 r2 then [ (snd r2 + 1L, snd r1) ]
    else [ r1 ]

let includeRange list range =
    list
    |> List.collect (fun r ->
        removeRange r range
    )
    |> List.append [ range ]

let countAllValidIds ranges =
    ([], ranges)
    ||> Seq.fold includeRange
    |> Seq.sumBy (fun (a, b) -> b - a + 1L)

File.ReadAllLines "input.txt"
|> parse
|> fst
|> countAllValidIds
|> printfn "Part 2: %d"
