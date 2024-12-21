open Checked
open System.IO
open System.Text.RegularExpressions

let getNumericPart (v: string) = Regex.Replace(v, @"\D", "") |> int

let moveNumeric a b =
    let coords = Map.ofList [
        '7', (0, 0)
        '8', (1, 0)
        '9', (2, 0)
        '4', (0, 1)
        '5', (1, 1)
        '6', (2, 1)
        '1', (0, 2)
        '2', (1, 2)
        '3', (2, 2)
        '0', (1, 3)
        'A', (2, 3)
    ]
    let (ax, ay) = coords.[a]
    let (bx, by) = coords.[b]
    [
        // avoid cell without button
        if ay = 3 && bx = 0 then
            if ay - by > 0 then (int64 (ay - by), '^')
            if ax - bx > 0 then (int64 (ax - bx), '<')
        // avoid cell without button
        elif ax = 0 && by = 3 then
            if bx - ax > 0 then (int64 (bx - ax), '>')
            if by - ay > 0 then (int64 (by - ay), 'v')
        else
            // avoid left button in subsequent layers
            // other buttons are reached without left button in subsequent layers
            if ax - bx > 0 then (int64 (ax - bx), '<')
            if ay - by > 0 then (int64 (ay - by), '^')
            if by - ay > 0 then (int64 (by - ay), 'v')
            if bx - ax > 0 then (int64 (bx - ax), '>')
        (1L, 'A')
    ]

let moveDirectional a b count =
    let coords = Map.ofList [
        '^', (1, 0)
        'A', (2, 0)
        '<', (0, 1)
        'v', (1, 1)
        '>', (2, 1)
    ]
    let (ax, ay) = coords.[a]
    let (bx, by) = coords.[b]
    [
        // avoid cell without button
        if ay = 0 && bx = 0 then
            if by - ay > 0 then (int64 (by - ay), 'v')
            if ax - bx > 0 then (int64 (ax - bx), '<')
        // avoid cell without button
        elif ax = 0 && by = 0 then
            if bx - ax > 0 then (int64 (bx - ax), '>')
            if ay - by > 0 then (int64 (ay - by), '^')
        else
            // avoid left button in subsequent layers
            // other buttons are reached without left button in subsequent layers
            if ax - bx > 0 then (int64 (ax - bx), '<')
            if ay - by > 0 then (int64 (ay - by), '^')
            if by - ay > 0 then (int64 (by - ay), 'v')
            if bx - ax > 0 then (int64 (bx - ax), '>')
        (count, 'A')
    ]

let printMovements v list =
    list
    |> List.map (fun buttons ->
        buttons
        |> List.map (fun (count, button) -> System.String(button, count))
        |> String.concat ""
    )
    |> String.concat " "
    |> fun s -> printfn "%s: %s (%d)" v s (list |> List.collect id |> List.sumBy fst)

let getMovementsForNumericKeyboard v =
    let rec fn v c acc =
        if v = "" then
            acc
            |> List.groupBy id
            |> List.map (fun (group, list) -> (group, int64 list.Length))
        else
            let c' = v.[0]
            fn v.[1..] c' (moveNumeric c c' :: acc)
    fn v 'A' []

let getMovementsForDirectionalKeyboard movementGroups =
    let rec fn group button acc =
        match group with
        | [] -> acc
        | (count, nextButton) :: vs ->
            let moves = moveDirectional button nextButton count
            fn vs nextButton (moves :: acc)
    [
        for (group, count) in movementGroups do
        for subGroup in fn group 'A' [] -> (subGroup, count)
    ]
    |> List.groupBy fst
    |> List.map (fun (group, list) -> (group, List.sumBy snd list))

let getSequenceLength directionalKeyboards v =
    let movements = getMovementsForNumericKeyboard v
    [1..directionalKeyboards]
    |> List.fold (fun movements _i -> getMovementsForDirectionalKeyboard movements) movements
    |> List.sumBy (fun (group, count) -> group |> List.sumBy (fun (c, _b) -> c * count))

File.ReadAllLines "input.txt"
|> Seq.sumBy (fun v -> getSequenceLength 2 v * int64 (getNumericPart v))
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.sumBy (fun v -> getSequenceLength 25 v * int64 (getNumericPart v))
|> printfn "Part 2: %d"
