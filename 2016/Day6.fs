let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day6-input.txt")

let length = input.[0].Length

let solution1 =
    Seq.init length (fun i ->
        input
        |> Seq.map (fun line -> line.[i])
        |> Seq.groupBy id
        |> Seq.maxBy (snd >> Seq.length)
        |> fst
    )
    |> Seq.toArray
    |> System.String

let solution2 =
    Seq.init length (fun i ->
        input
        |> Seq.map (fun line -> line.[i])
        |> Seq.groupBy id
        |> Seq.minBy (snd >> Seq.length)
        |> fst
    )
    |> Seq.toArray
    |> System.String
    