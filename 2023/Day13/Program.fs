open System.IO

let parsePatterns (lines: string array) =
    lines
    |> Seq.indexed
    |> Seq.filter (fun (i, line) -> line = "")
    |> Seq.map fst
    |> fun v -> [ -1; yield! v; lines.Length ]
    |> Seq.pairwise
    |> Seq.map (fun (startLine, endLine) ->
        let rows = endLine - startLine - 1
        let cols = lines.[startLine + 1].Length
        Array2D.init rows cols (fun row column -> lines.[startLine + 1 + row].[column])
    )
    |> Seq.toList

let getRow pattern index =
    [ for col in [0..Array2D.length2 pattern - 1] -> pattern.[index, col] ]

let getColumn pattern index =
    [ for row in [0..Array2D.length1 pattern - 1] -> pattern.[row, index] ]

let tryFindMirrorLine (list: _ list) =
    Seq.init (list.Length / 2) id
    |> Seq.filter (fun length ->
        let left = List.take (length + 1) list
        let right = List.skip left.Length list |> List.take left.Length |> List.rev
        left = right
    )
    |> Seq.tryLast
    |> function
    | Some index -> Some index
    | _ -> None

let rows pattern =
    [ for i in 0 .. Array2D.length1 pattern - 1 -> getRow pattern i ]

let columns pattern =
    [ for i in 0 .. Array2D.length2 pattern - 1 -> getColumn pattern i ]

let findMirrorLines pattern =
    [
        yield! pattern |> columns |> tryFindMirrorLine |> Option.map (fun position -> position + 1) |> Option.toList
        yield! pattern |> columns |> List.rev |> tryFindMirrorLine |> Option.map (fun position -> Array2D.length2 pattern - position - 1) |> Option.toList
        yield! pattern |> rows |> tryFindMirrorLine |> Option.map (fun position -> (position + 1) * 100) |> Option.toList
        yield! pattern |> rows |> List.rev |> tryFindMirrorLine |> Option.map (fun position -> (Array2D.length1 pattern - position - 1) * 100) |> Option.toList
    ]

let printPattern pattern =
    pattern
    |> Seq.cast<char>
    |> Seq.chunkBySize (Array2D.length2 pattern)
    |> Seq.map System.String
    |> String.concat "\n"

let findMirrorLine pattern =
    match findMirrorLines pattern with
    | [ v ] -> v
    | [] -> failwith $"No mirror line found for pattern:\n%s{printPattern pattern}"
    | _ -> failwith $"Multiple mirror lines found for pattern:\n%s{printPattern pattern}"

File.ReadAllLines "input.txt"
|> parsePatterns
|> Seq.sumBy findMirrorLine
|> printfn "Part 1: %d"

let findMirrorLineWithSmudge pattern =
    let originalMirrorLine = findMirrorLine pattern
    [
        for row in 0..Array2D.length1 pattern - 1 do
        for col in 0..Array2D.length2 pattern - 1 do
            let newChar = if pattern.[row, col] = '#' then '.' else '#'
            let swapped = Array2D.copy pattern
            Array2D.set swapped row col newChar
            swapped
    ]
    |> Seq.tryPick (fun pattern ->
        match findMirrorLines pattern |> List.except [ originalMirrorLine ] with
        | [ v ] -> Some v
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failwith $"No smudge found for pattern:\n%s{printPattern pattern}")

File.ReadAllLines "input.txt"
|> parsePatterns
|> Seq.sumBy findMirrorLineWithSmudge
|> printfn "Part 2: %d"
