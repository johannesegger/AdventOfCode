type Position = {
    Row: int
    Column: int
}

type Pattern = {
    Size: int
    Positions: Position Set
}

module Pattern =
    let parse (input: string) =
        let rows = input.Split('/')
        let positions =
            rows
            |> Seq.indexed
            |> Seq.collect (fun (rowIdx, row) ->
                row
                |> Seq.indexed
                |> Seq.choose (fun (columnIdx, c) ->
                    if c = '#'
                    then Some { Row = rowIdx; Column = columnIdx }
                    else None
                )
            )
            |> Set.ofSeq
        {
            Size = rows.Length
            Positions = positions
        }

    let toString pattern =
        Seq.init pattern.Size (fun row ->
            Seq.init pattern.Size (fun column ->
                if Set.contains { Row = row; Column = column } pattern.Positions
                then "#"
                else "."
            )
            |> String.concat ""
        )
        |> String.concat "/"

    let mapPositions fn pattern =
        let positions = Set.map fn pattern.Positions
        { pattern with Positions = positions }

    let flipVertical pattern =
        let flipPosition p =
            { p with Row = pattern.Size - 1 - p.Row }
        pattern |> mapPositions flipPosition

    let rotateClockwise pattern =
        let flipPosition p =
            { p with Row = p.Column; Column = pattern.Size - 1 - p.Row }
        pattern |> mapPositions flipPosition

    let getEqualPatterns pattern =
        [
            pattern
            pattern |> flipVertical
            pattern |> rotateClockwise
            pattern |> rotateClockwise |> flipVertical
            pattern |> rotateClockwise |> rotateClockwise
            pattern |> rotateClockwise |> rotateClockwise |> flipVertical
            pattern |> rotateClockwise |> rotateClockwise |> rotateClockwise
            pattern |> rotateClockwise |> rotateClockwise |> rotateClockwise |> flipVertical
        ]
        |> Set.ofList

module Rule =
    let parse line =
        let m = System.Text.RegularExpressions.Regex.Match(line, @"^(?<pattern>\S+)\s*=>\s*(?<replacement>\S+)$")
        if not m.Success then failwithf "Line can't be parsed: '%s'" line
        
        Pattern.parse m.Groups.["pattern"].Value |> Pattern.getEqualPatterns
        , Pattern.parse m.Groups.["replacement"].Value

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day21-input.txt")
    |> Seq.map Rule.parse
    |> Seq.collect (fun (patterns, replacement) ->
        patterns |> Seq.map (fun p -> p, replacement)
    )
    |> Map.ofSeq

let startPattern = Pattern.parse ".#./..#/###"

let moveToZero (position, splitSize) positions =
    positions
    |> Set.map (fun p ->
        { p with
            Row = p.Row - position.Row * splitSize
            Column = p.Column - position.Column * splitSize
        })

let moveToPosition (position, splitSize) positions =
    positions
    |> Set.map (fun p ->
        { p with
            Row = p.Row + position.Row * splitSize
            Column = p.Column + position.Column * splitSize
        })

let split pattern =
    let splitSize =
        if pattern.Size % 2 = 0 then 2 else 3
    let count = pattern.Size / splitSize

    let lookup =
        pattern.Positions
        |> Set.toSeq
        |> Seq.groupBy (fun p -> p.Row / splitSize, p.Column / splitSize)
        |> Map.ofSeq
    Seq.init count (fun row ->
        Seq.init count (fun column ->
            let positions =
                Map.tryFind (row, column) lookup
                |> Option.defaultValue Seq.empty
                |> Set.ofSeq
            let position = { Row = row; Column = column }

            position,
            {
                Size = splitSize
                Positions = moveToZero (position, splitSize) positions
            }
        )
    )
    |> Seq.collect id

let replace (position, pattern) =
    let replacement = Map.find pattern input

    { replacement with
        Positions = moveToPosition (position, replacement.Size) replacement.Positions
    }

let iterate pattern =
    let replacedPatterns =
        pattern
        |> split
        |> Seq.map (fun p -> fst p, replace p) 
        |> Seq.toList
    let size =
        replacedPatterns
        |> List.filter (fst >> (fun p -> p.Row = 0))
        |> List.sumBy (snd >> (fun p -> p.Size))
    {
        Size = size
        Positions =
            replacedPatterns
            |> Seq.collect (snd >> (fun p -> p.Positions))
            |> Set.ofSeq
    }

let solve iterations =
    Seq.init iterations ignore
    |> Seq.fold (fun pattern () -> iterate pattern) startPattern
    |> fun p -> p.Positions
    |> Set.count

let solution1 = solve 5

let solution2 = solve 18
