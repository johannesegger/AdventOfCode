let parse (line: string) =
    line
    |> Seq.mapi (fun i c -> i, c)
    |> Seq.choose (fun (i, c) -> if c = '[' || c = ']' then Some i else None)
    |> Seq.append [-1]
    |> fun s -> Seq.append s [line.Length]
    |> Seq.pairwise
    |> Seq.mapi (fun i (idx1, idx2) ->
        let from = idx1 + 1
        i % 2 = 0, line.Substring(from, idx2 - from))
    |> Seq.toList
    |> List.partition fst
    |> fun (p1, p2) -> List.map snd p1, List.map snd p2

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day7-input.txt")
    |> Array.map parse


let isFourLetterAbba (text: string) =
    if text.[0] = text.[1]
    then false
    else
        let length = 2
        let left = text.Substring(0, length)
        let right =
            text.Substring length
            |> Seq.rev
            |> Seq.toArray
            |> System.String
        left = right

let getSubstrings numberOfLetters (text: string) =
    let numberOfSubstrings = text.Length - numberOfLetters + 1
    Seq.init numberOfSubstrings (fun i -> text.Substring(i, numberOfLetters))

let hasFourLetterAbba (text: string) =
    getSubstrings 4 text
    |> Seq.exists isFourLetterAbba

let solution1 =
    input
    |> Seq.filter (fun (outside, inside) ->
        inside
        |> Seq.exists hasFourLetterAbba
        |> not
        &&
        outside
        |> Seq.exists hasFourLetterAbba
    )
    |> Seq.length

let isABA (text: string) =
    text.[0] <> text.[1] && text.[0] = text.[2]

let getABAs (text: string) =
    getSubstrings 3 text
    |> Seq.filter isABA
    |> Seq.toList

let getBAB (aba: string) =
    System.String([| aba.[1]; aba.[0]; aba.[1] |])

let solution2 =
    input
    |> Seq.filter (fun (outside, inside) ->
        let outsideABAs = List.collect getABAs outside |> List.distinct
        let insideABAs = List.collect getABAs inside |> List.distinct |> List.map getBAB
        Seq.allPairs outsideABAs insideABAs
        |> Seq.exists (fun (outsideABA, insideABA) -> outsideABA = insideABA)

    )
    |> Seq.length
