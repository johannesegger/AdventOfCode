open Checked
open System.IO

let parse lines =
    let rules = lines |> Array.takeWhile ((<>) "") |> Array.map (fun line -> let parts = line.Split('|') in int parts.[0], int parts.[1])
    let updates = lines |> Array.skip (rules.Length + 1) |> Array.map (fun update -> update.Split(',') |> Array.map int)
    (rules, updates)

let isNotBeforeAny rules page pages =
    rules
    |> Seq.forall (fun (a, b) ->
        pages |> Seq.forall (fun a' -> (a, b) <> (page, a'))
    )

let isCorrect rules update =
    [
        for i in 0..(Array.length update) - 1 do
            (Array.take i update, Array.item i update)
    ]
    |> List.forall (fun (previousPages, page) ->
        isNotBeforeAny rules page previousPages
    )

let getMiddlePageNumber (update: 'a[]) =
    update.[update.Length / 2]

let getMiddlePageNumbersOfCorrectUpdates (rules, updates) =
    updates
    |> Seq.filter (isCorrect rules)
    |> Seq.map getMiddlePageNumber

File.ReadAllLines "input.txt"
|> parse
|> getMiddlePageNumbersOfCorrectUpdates
|> Seq.sum
|> printfn "Part 1: %d"

let rec isBefore rules a b =
    rules |> Seq.exists ((=) (a, b))

let fixOrder rules update =
    update
    |> Array.sortWith (fun a b -> if isBefore rules a b then -1 else 1)

let getMiddlePageNumbersOfCorrectedUpdates (rules, updates) =
    updates
    |> Seq.filter (isCorrect rules >> not)
    |> Seq.map (fixOrder rules)
    |> Seq.map getMiddlePageNumber

File.ReadAllLines "input.txt"
|> parse
|> getMiddlePageNumbersOfCorrectedUpdates
|> Seq.sum
|> printfn "Part 2: %d"
