type Position = {
    X: int
    Y: int
}

type Instruction =
    | Rect of int * int
    | RotateRow of int * int
    | RotateColumn of int * int

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success
    then
        m.Groups
        |> Seq.cast<System.Text.RegularExpressions.Group>
        |> Seq.skip 1
        |> Seq.map (fun p -> p.Value)
        |> Seq.toList
        |> Some
    else None

let parse = function
    | Regex @"^rect (\d+)x(\d+)$" [width; height] -> Rect (int width, int height)
    | Regex @"^rotate row y=(\d+) by (\d+)$" [row; amount] -> RotateRow (int row, int amount)
    | Regex @"^rotate column x=(\d+) by (\d+)$" [column; amount] -> RotateColumn (int column, int amount)
    | s -> failwithf "Unrecognized instruction: \"%s\"" s 

let (%%) n1 n2 = ((n1 % n2) + n2) % n2

let width = 50
let height = 6

let applyInstruction positions = function
    | Rect (width, height) ->
        [ for x in 0..(width - 1) do for y in 0..(height - 1) -> { X = x; Y = y } ]
        |> List.append positions
        |> List.distinct
    | RotateRow (y, amount) ->
        positions
        |> List.map (fun p ->
            if p.Y = y
            then { p with X = (p.X + amount) %% width }
            else p
        )
    | RotateColumn (x, amount) ->
        positions
        |> List.map (fun p ->
            if p.X = x
            then { p with Y = (p.Y + amount) %% height }
            else p
        )

let result =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day8-input.txt")
    |> Seq.map parse
    |> Seq.fold applyInstruction List.empty

let solution1 =
    result
    |> List.length

let solution2 =
    let map =
        result
        |> Seq.map (fun p -> (p.X, p.Y), "X")
        |> Map.ofSeq

    [ 0..(height - 1) ]
    |> List.map (fun y ->
        [ 0..(width - 1) ]
        |> List.map (fun x ->
            match Map.tryFind (x, y) map with
            | Some p -> p
            | _ -> " "
        )
        |> String.concat ""
    )
    |> String.concat System.Environment.NewLine
