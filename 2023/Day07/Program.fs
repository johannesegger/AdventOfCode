open System.IO

let parseCard = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | n when n >= '2' && n <= '9' -> int (n - '0')
    | n -> failwith $"Invalid card: %c{n}"

let parseCards cards =
    cards
    |> Seq.map parseCard
    |> Seq.toList

let parseHand (line: string) =
    let parts = line.Split()
    (parseCards parts.[0], int parts.[1])

let (highCard, onePair, twoPair, threeOfAKind, fullHouse, fourOfAKind, fiveOfAKind) =
    (1, 2, 3, 4, 5, 6, 7)

let getStrength cards =
    let groups = cards |> List.countBy id
    if groups.Length = 1 then fiveOfAKind
    elif groups |> List.exists (snd >> (=) 4) then fourOfAKind
    elif groups.Length = 2 then fullHouse
    elif groups |> List.exists (snd >> (=) 3) then threeOfAKind
    elif groups.Length = 3 then twoPair
    elif groups.Length = 4 then onePair
    else highCard

let getScore (cards, bid) =
    let strength = getStrength cards
    (strength, cards)

File.ReadAllLines "input.txt"
|> Seq.map parseHand
|> Seq.sortBy getScore
|> Seq.mapi (fun i (cards, bid) -> (i + 1) * bid)
|> Seq.sum
|> printfn "Part 1: %d"

let joker = 11

let getStrengthWithJokers cards =
    let groups = cards |> List.countBy id
    let jokers = cards |> List.filter ((=) joker) |> List.length

    if groups.Length = 1 then fiveOfAKind // 5
    elif groups |> List.exists (snd >> (=) 4) then // 4 + 1
        if jokers > 0 then fiveOfAKind else fourOfAKind
    elif groups.Length = 2 then // 3 + 2
        if jokers > 0 then fiveOfAKind else fullHouse
    elif groups |> List.exists (snd >> (=) 3) then // 3 + 1 + 1
        if jokers > 0 then fourOfAKind else threeOfAKind
    elif groups.Length = 3 then // 2 + 2 + 1
        if jokers = 2 then fourOfAKind
        elif jokers = 1 then fullHouse
        else twoPair
    elif groups.Length = 4 then // 2 + 1 + 1 + 1
        if jokers > 0 then threeOfAKind
        else onePair
    else // 1 + 1 + 1 + 1 + 1
        if jokers > 0 then onePair
        else highCard

let getScoreWithJokers (cards, bid) =
    let strength = getStrengthWithJokers cards
    let highCards = cards |> List.map (fun c -> if c = joker then 1 else c)
    (strength, highCards)

File.ReadAllLines "input.txt"
|> Seq.map parseHand
|> Seq.sortBy getScoreWithJokers
|> Seq.mapi (fun i (cards, bid) -> (i + 1) * bid)
|> Seq.sum
|> printfn "Part 2: %d"
