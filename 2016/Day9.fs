let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\Day9-input.txt")

let pos v = System.Math.Max(0, v)

let tryFindMarker (text: string) (startIndex: int) (endIndex: int) =
    match text.IndexOf("(", startIndex, pos(endIndex - startIndex - 1)) with
    | -1 -> None
    | i ->
        let regex = System.Text.RegularExpressions.Regex @"\((?<l>\d+)x(?<r>\d+)\)"
        let m = regex.Match(text, i, pos(endIndex - i - 1))
        if not m.Success then failwith "Didn't expect no match"
        Some (i, m.Length, int m.Groups.["l"].Value, int m.Groups.["r"].Value)

let solution1 =
    let folder (text: string) (idx: int, length) _ =
        match tryFindMarker text idx text.Length with
        | Some (idx, markerLength, substringLength, repetitions) ->
            let nextIdx = idx + markerLength + substringLength
            let nextLength = length - markerLength + (substringLength * (repetitions - 1))
            nextIdx, nextLength
        | None -> -1, length

    Seq.initInfinite ignore
    |> Seq.scan (folder input) (0, input.Length)
    |> Seq.takeWhile (fst >> ((<>) -1))
    |> Seq.last
    |> snd

let solution2 =
    let rec folder (text: string) (startIndex: int, endIndex: int, length: int64) _ =
        match tryFindMarker text startIndex endIndex with
        | Some (idx, markerLength, substringLength, repetitions) ->
            let subStartIndex = idx + markerLength
            let subEndIndex = idx + markerLength + substringLength

            let (_, _, subLength) =
                Seq.initInfinite ignore
                |> Seq.scan (folder text) (subStartIndex, subEndIndex, int64 substringLength)
                |> Seq.takeWhile (fun (startIndex, _, _) -> startIndex <> -1)
                |> Seq.last

            let nextLength = length - (int64 substringLength) - (int64 markerLength) + subLength * (int64 repetitions)
            subEndIndex, endIndex, nextLength
        | None -> -1, -1, length

    Seq.initInfinite ignore
    |> Seq.scan (folder input) (0, input.Length, int64 input.Length)
    |> Seq.takeWhile (fun (startIndex, _, _) -> startIndex <> -1)
    |> Seq.last
    |> fun (_, _, l) -> l
