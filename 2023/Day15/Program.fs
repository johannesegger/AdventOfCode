open System.IO

let parseSequence (text: string) =
    text.Split(',') |> Seq.toList

let getHash text =
    (0, text)
    ||> Seq.fold (fun hash c ->
        (hash + int c) * 17 % 256
    )

File.ReadAllText "input.txt"
|> parseSequence
|> List.sumBy getHash
|> printfn "Part 1: %d"

type Operation = RemoveLens of string | AddLens of string * int

let parseOperation (text: string) =
    if text.EndsWith "-" then RemoveLens (text.Substring(0, text.Length - 1))
    else
        let parts = text.Split("=")
        AddLens (parts.[0], int parts.[1])

let parseOperations (text: string) =
    text.Split(',') |> Seq.map parseOperation |> Seq.toList

let runOperation (boxes: _ array) operation =
    match operation with
    | RemoveLens label ->
        let boxNumber = getHash label
        let lenses = boxes.[boxNumber] |> List.filter (fst >> (<>) label)
        Array.updateAt boxNumber lenses boxes
    | AddLens (label, focalLength) ->
        let boxNumber = getHash label
        let lenses = boxes.[boxNumber]
        let lenses' =
            match lenses |> List.tryFindIndex (fst >> (=) label) with
            | Some lensIndex -> List.updateAt lensIndex (label, focalLength) lenses
            | None -> lenses @ [ (label, focalLength) ]
        Array.updateAt boxNumber lenses' boxes

let runOperations operations =
    List.fold runOperation (Array.replicate 256 []) operations

let getBoxFocusingPower (boxNumber, lenses) =
    lenses
    |> List.mapi (fun i (_, focalLength) -> (boxNumber + 1) * (i + 1) * focalLength)
    |> List.sum

let getFocusingPower boxes =
    boxes |> Array.indexed |> Seq.sumBy getBoxFocusingPower

File.ReadAllText "input.txt"
|> parseOperations
|> runOperations
|> getFocusingPower
|> printfn "Part 2: %d"
