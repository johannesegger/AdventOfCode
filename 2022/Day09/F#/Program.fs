open System
open System.IO

let moveTail (hx: int, hy: int) (tx: int, ty: int) =
    if Math.Abs(hx - tx) <= 1 && Math.Abs(hy - ty) <= 1 then (tx, ty)
    else
        let dx = Math.Sign(hx - tx)
        let dy = Math.Sign(hy - ty)
        ((tx + dx), (ty + dy))

let moveTails knots dhx dhy =
    ([], knots)
    ||> List.fold (fun state (tx, ty) ->
        match state with
        | [] -> (tx + dhx, ty + dhy) :: state
        | h :: xs -> moveTail h (tx, ty) :: h :: xs
    )
    |> List.rev

let rec moveHead visitedPositions delta knots =
    match delta with
    | (0, 0) -> (visitedPositions, knots)
    | (x, 0) ->
        let step = Math.Sign(x)
        let knots' = moveTails knots step 0
        let visitedPositions' = Set.add (List.last knots') visitedPositions
        moveHead visitedPositions' (x - step, 0) knots'
    | (0, y) ->
        let step = Math.Sign(y)
        let knots' = moveTails knots 0 step
        let visitedPositions' = Set.add (List.last knots') visitedPositions
        moveHead visitedPositions' (0, y - step) knots'
    | _ -> failwith "Move in both directions not allowed"

let move (visitedPositions, knots) (motion: string) =
    match motion.Split(' ') with
    | [| "L"; x |] -> moveHead visitedPositions (-(int x), 0) knots
    | [| "R"; x |] -> moveHead visitedPositions ((int x), 0) knots
    | [| "U"; x |] -> moveHead visitedPositions (0, (int x)) knots
    | [| "D"; x |] -> moveHead visitedPositions (0, -(int x)) knots
    | _ -> failwith $"Invalid motion: {motion}"

let motions = File.ReadAllLines("input.txt")
((Set.ofList [(0, 0)], List.replicate 2 (0, 0)), motions)
||> Seq.fold move
|> fun (visitedPositions, _) -> Set.count visitedPositions
|> printfn "Part 1: %d"

((Set.ofList [(0, 0)], List.replicate 10 (0, 0)), motions)
||> Seq.fold move
|> fun (visitedPositions, _) -> Set.count visitedPositions
|> printfn "Part 2: %d"
