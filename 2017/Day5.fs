let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day5-input.txt")
    |> Seq.mapi (fun idx i -> idx, int i)
    |> Map.ofSeq

let jumpUntilEscaped jumper =
    Seq.initInfinite ignore
    |> Seq.scan (fun (instructions, i) () -> jumper instructions i) (input, 0)
    |> Seq.mapi (fun idx (instructions, i) -> idx, Map.containsKey i instructions |> not)
    |> Seq.choose (fun (idx, isEscaped) -> if isEscaped then Some idx else None)
    |> Seq.head

let solution1 =
    let jump instructions i =
        let offset = Map.find i instructions
        instructions |> Map.add i (offset + 1)
        , i + offset
    jumpUntilEscaped jump

let solution2 =
    let jump instructions i =
        let offset = Map.find i instructions
        let delta = if offset >= 3 then -1 else 1
        instructions |> Map.add i (offset + delta)
        , i + offset
    jumpUntilEscaped jump
