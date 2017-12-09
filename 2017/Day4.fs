let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day4-input.txt")
    |> Seq.map (fun line -> line.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries))

let solution1 =
    input
    |> Seq.filter (fun phrases -> phrases |> Set.ofArray |> Set.count = phrases.Length)
    |> Seq.length

let solution2 =
    input
    |> Seq.filter (fun phrases ->
        let parts =
            phrases
            |> Seq.map (fun p ->
                p
                |> Seq.groupBy id
                |> Seq.map (fun (key, group) -> key, Seq.length group)
                |> Map.ofSeq
            )
            |> Set.ofSeq
            |> Set.count
        parts = phrases.Length
    )
    |> Seq.length
