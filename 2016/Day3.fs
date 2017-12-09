let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day3-input.txt")

let values =
    input
    |> Seq.map (fun line ->
        let v =
            line.Split([| " " |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        v.[0], v.[1], v.[2]
    )

let numberOfValidTriangles =
    Seq.map (fun (s1, s2, s3) ->
        let s = [| s1; s2; s3 |] |> Array.sort
        s.[0], s.[1], s.[2]
    )
    >> Seq.where (fun (s1, s2, s3) -> s1 + s2 > s3)
    >> Seq.length

let solution1 =
    values
    |> numberOfValidTriangles

let solution2 =
    values
    |> Seq.chunkBySize 3
    |> Seq.collect (fun chunk ->
        let (a1, a2, a3) = chunk.[0]
        let (b1, b2, b3) = chunk.[1]
        let (c1, c2, c3) = chunk.[2]
        [
            a1, b1, c1
            a2, b2, c2
            a3, b3, c3
        ]
    )
    |> numberOfValidTriangles
