open System
open System.IO
open System.Text.RegularExpressions

let parseCard text =
    let m = Regex.Match(text, @"^Card\s+\d+: ([^\|]+)\|([^$]+)$")
    let winningNumbers = m.Groups.[1].Value.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Set.ofSeq
    let ownNumbers = m.Groups.[2].Value.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Set.ofSeq
    (winningNumbers, ownNumbers)

let getMatchCount (winningNumbers, ownNumbers) =
    Set.intersect winningNumbers ownNumbers
    |> Set.count

File.ReadAllLines "input.txt"
|> Seq.map (parseCard >> getMatchCount)
|> Seq.sumBy (fun v -> if v = 0 then 0 else 2.**(float v - 1.) |> int)
|> printfn "Part 1: %d"

let winCards count amount cards =
    cards
    |> List.mapi (fun index (cardAmount, card) ->
        if index < count then (cardAmount + amount, card)
        else (cardAmount, card)
    )

let getCardCopies cards =
    let rec fn cards totalCards =
        match cards with
        | [] -> totalCards
        | (cardCount, card) :: remainingCards ->
            let matchCount = getMatchCount card
            let remainingCards' = remainingCards |> winCards matchCount cardCount
            let totalCards' = (cardCount, card) :: totalCards
            fn remainingCards' totalCards'

    let initialCards =
        cards
        |> Seq.map (fun card -> (1, card))
        |> Seq.toList
    fn initialCards []

File.ReadAllLines "input.txt"
|> Seq.map parseCard
|> getCardCopies
|> List.sumBy fst
|> printfn "Part 2: %d"
