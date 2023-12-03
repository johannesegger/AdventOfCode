open System.IO

let countBits bits =
    let zeros = bits |> Seq.filter ((=) '0') |> Seq.length
    let ones = bits |> Seq.filter ((=) '1') |> Seq.length
    (zeros, ones)

let calculateGammaAndEpsilonRate numbers =
    ((0, 0), Seq.transpose numbers)
    ||> Seq.fold (fun (a, b) bits ->
        let (zeros, ones) = countBits bits
        let gammaRateBit = if zeros > ones then 0 else 1
        let epsilonRateBit = if zeros < ones then 0 else 1
        (a * 2 + gammaRateBit, b * 2 + epsilonRateBit)
    )

File.ReadAllLines "input.txt"
|> calculateGammaAndEpsilonRate
|> fun (a, b) -> a * b
|> printfn "Part 1: %d"

let calculateOxygenGeneratorAndCO2ScrubberRating numbers =
    let rec getRating numbers keepFn index =
        match numbers with
        | [ v ] -> System.Convert.ToInt32(v, fromBase = 2)
        | vs ->
            let (zeros, ones) = vs |> Seq.map (fun v -> v.[index]) |> countBits
            let keep = keepFn zeros ones
            let numbers' = numbers |> List.filter (fun v -> v.[index] = keep)
            getRating numbers' keepFn (index + 1)
    let a = getRating numbers (fun zeros ones -> if zeros > ones then '0' else '1') 0
    let b = getRating numbers (fun zeros ones -> if zeros > ones then '1' else '0') 0
    (a, b)

File.ReadAllLines "input.txt"
|> List.ofArray
|> calculateOxygenGeneratorAndCO2ScrubberRating
|> fun (a, b) -> a * b
|> printfn "Part 2: %d"
