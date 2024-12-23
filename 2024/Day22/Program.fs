open Checked
open System.IO

let calculateNextSecretNumber n =
    let n = ((n * 64L) ^^^ n) % 16777216L
    let n = ((n / 32L) ^^^ n) % 16777216L
    let n = ((n * 2048L) ^^^ n) % 16777216L
    n

let calculateSecretNumbers number =
    let rec fn n acc =
        if List.length acc = 2000 then List.rev acc
        else
            let n = calculateNextSecretNumber n
            fn n (n :: acc)

    number :: fn number []

File.ReadAllLines "input.txt"
|> Seq.sumBy (int64 >> calculateSecretNumbers >> List.last)
|> printfn "Part 1: %d"

let getLastDigit n = int (n % 10L)

let getChangesId (a, b, c, d) =
    // use ints for map lookup because of performance
    // (a, b, c, d)
    // $"%d{a},%d{b},%d{c},%d{d}"
    (((((a + 10) * 20 + (b + 10)) * 20) + (c + 10)) * 20) + (d + 10)

let getPriceChanges list =
    let rec fn price prices changes acc =
        match prices with
        | [] -> acc
        | price' :: prices' ->
            let change = price' - price
            let (changes', acc') =
                match changes with
                | (_a, Some b, Some c, Some d) ->
                    (Some b, Some c, Some d, Some change),
                    Map.change (getChangesId (b, c, d, change)) (Option.defaultValue price' >> Some) acc
                | (_a, b, c, d) ->
                    (b, c, d, Some change), acc
            fn price' prices' changes' acc'
    fn (List.head list) (List.tail list) (None, None, None, None) Map.empty

let calculateMaxBananas (allBuyersPriceChanges: _ list) =
    let distinctPriceChanges =
        allBuyersPriceChanges
        |> Seq.collect (Map.toSeq >> Seq.map fst)
        |> Seq.distinct
    distinctPriceChanges
    |> Seq.mapi (fun i priceChanges ->
        allBuyersPriceChanges
        |> Seq.sumBy (Map.tryFind priceChanges >> Option.defaultValue 0)
    )
    |> Seq.max

File.ReadAllLines "input.txt"
|> Seq.map (int64 >> calculateSecretNumbers >> List.map getLastDigit >> getPriceChanges)
|> Seq.toList
|> calculateMaxBananas
|> printfn "Part 2: %d"
