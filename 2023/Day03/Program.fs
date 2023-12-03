open System.IO

let readNumbers line =
    let (numbers, number) =
        (([], None), Seq.indexed line)
        ||> Seq.fold (fun (numbers, currentNumber) (i, c) ->
            match c, currentNumber with
            | c, Some (startIndex, number) when c >= '0' && c <= '9' -> (numbers, Some (startIndex, number * 10 + int (c - '0')))
            | c, None when c >= '0' && c <= '9' -> (numbers, Some (i, int (c - '0')))
            | c, Some number -> number :: numbers, None
            | c, None -> numbers, None
        )
    match number with
    | Some v -> v :: numbers
    | None -> numbers

let getLineNumbers row line =
    readNumbers line
    |> Seq.map (fun (col, number) -> ((row, col), number))

let getNumbers lines =
    lines
    |> Seq.mapi getLineNumbers
    |> Seq.collect id

let getNeighborCells (lines: string array) ((row, col), number) =
    let width = $"%d{number}".Length
    [
        for i in -1..width do (row - 1, col + i)
        for i in -1..width do (row + 1, col + i)
        (row, col - 1)
        (row, col + width)
    ]
    |> List.filter (fun (row, col) -> row >= 0 && row < lines.Length && col >= 0 && col < lines.[0].Length)

let getSumOfPartNumbers lines =
    let numbers = getNumbers lines
    numbers
    |> Seq.filter (fun number ->
        getNeighborCells lines number
        |> Seq.exists (fun (row, col) -> lines.[row].[col] <> '.')
    )
    |> Seq.sumBy (fun (_position, number) -> number)

File.ReadAllLines("input.txt")
|> getSumOfPartNumbers
|> printfn "Part 1: %d"

let getSumOfGearRatios lines =
    let numbers = getNumbers lines

    [
        for (row, line) in Seq.indexed lines do
        for (col, c) in Seq.indexed line -> ((row, col), c)
    ]
    |> Seq.filter (fun (_position, c) -> c = '*')
    |> Seq.sumBy (fun (position, c) ->
        let gearParts =
            numbers
            |> Seq.filter (fun number ->
                getNeighborCells lines number |> List.contains position
            )
            |> Seq.map snd
            |> Seq.toList
        match gearParts with
        | [a; b] -> a * b
        | _ -> 0
    )

File.ReadAllLines("input.txt")
|> getSumOfGearRatios
|> printfn "Part 2: %d"
