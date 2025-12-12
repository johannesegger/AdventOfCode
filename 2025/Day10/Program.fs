open Checked
open System.IO

let parseMachine (line: string) =
    let parts = line.Split()
    let targetState =
        (parts.[0].Trim('[', ']'), 0)
        ||> Seq.foldBack (fun c n ->
            if c = '#' then (n <<< 1) + 1
            else n <<< 1
        )
    let buttons =
        parts.[1..parts.Length - 2]
        |> Seq.map (fun v ->
            (0, v.Trim('(', ')').Split(','))
            ||> Seq.fold (fun n i ->
                n ||| (1 <<< int i)
            )
        )
        |> Seq.toList
    let joltageLevels =
        parts.[parts.Length - 1].Trim('{', '}').Split(',')
        |> Seq.map int
        |> Seq.toArray
    (targetState, buttons, joltageLevels)

let pushButton state button =
    state ^^^ button

let getMinNumberOfStepsToReachTargetState (targetState, buttons, _) =
    let rec fn states cnt =
        if states |> List.exists ((=) targetState) then cnt
        else
            let states' =
                states
                |> List.collect (fun state ->
                    buttons
                    |> List.map (pushButton state)
                )
            fn states' (cnt + 1)

    let initialState = 0
    fn [ initialState ] 0

File.ReadAllLines "input.txt"
|> Seq.sumBy (parseMachine >> getMinNumberOfStepsToReachTargetState)
|> printfn "Part 1: %d"

// TODO Part 2 seems to be correct, but way too slow

let getPossiblePushes targetJoltageLevels btn =
    let maxPushes =
        targetJoltageLevels
        |> Seq.indexed
        |> Seq.choose (fun (i, v) ->
            if btn >>> i &&& 1 = 1 then Some v else None
        )
        |> Seq.min
    [0..maxPushes]
    |> List.map (fun pushCount ->
        let targetJoltageLevels =
            targetJoltageLevels
            |> Seq.indexed
            |> Seq.map (fun (i, targetJoltageLevel) ->
                let singlePush = btn >>> i &&& 1
                targetJoltageLevel - singlePush * pushCount
            
            )
            |> Seq.toArray
        (pushCount, targetJoltageLevels)
    )
    |> List.rev

let getMinNumberOfStepsToReachJoltageLevels (_, buttons, targetJoltageLevels) =
    printfn "%A" targetJoltageLevels
    let rec fn remainingJoltageLevels remainingButtons totalButtonPushes minTotalPushes =
        if remainingJoltageLevels |> Array.forall ((=) 0) then
            printfn "========== %d" totalButtonPushes
            Some totalButtonPushes
        else
            match remainingButtons with
            | [] -> minTotalPushes
            | btn :: btns ->
                // getPossiblePushes remainingJoltageLevels btn
                // |> List.choose (fun (cnt, remainingJoltageLevels) ->
                //     fn remainingJoltageLevels btns (totalButtonPushes + cnt)
                // )
                // |> function
                // | [] -> None
                // | x -> Some (List.min x)
                (minTotalPushes, getPossiblePushes remainingJoltageLevels btn)
                ||> Seq.fold (fun minTotalPushes (cnt, remainingJoltageLevels) ->
                    match minTotalPushes with
                    | None -> fn remainingJoltageLevels btns (totalButtonPushes + cnt) None
                    | Some minTotalPushes when minTotalPushes <= totalButtonPushes + cnt -> (Some minTotalPushes)
                    | Some minTotalPushes -> fn remainingJoltageLevels btns (totalButtonPushes + cnt) (Some minTotalPushes)
                )

    let buttons =
        buttons
        |> List.sortByDescending (fun (b: int) -> System.Convert.ToString(b, 2) |> Seq.filter ((=)'1') |> Seq.length)
    fn targetJoltageLevels buttons 0 None
    |> Option.get

let pushButton2 state button =
    state
    |> Array.mapi (fun i v ->
        if button >>> i &&& 1 = 1 then v - 1 else v
    )

let getMinNumberOfStepsToReachJoltageLevels2 (_, buttons, targetJoltageLevels) =
    printfn "%A" targetJoltageLevels
    let emptyJoltageLevels = Array.zeroCreate (Array.length targetJoltageLevels)
    let rec fn states cnt =
        printfn "%d - %d" cnt (Set.count states)
        if states |> Set.contains emptyJoltageLevels then cnt
        else
            let states =
                states
                |> Seq.collect (fun s -> buttons |> Seq.map (pushButton2 s))
                |> Seq.filter (fun v-> v |> Seq.forall (fun v -> v >= 0))
                |> Set.ofSeq
            fn states (cnt + 1)
    fn (Set.singleton targetJoltageLevels) 0

File.ReadAllLines "input.txt"
|> Seq.sumBy (parseMachine >> getMinNumberOfStepsToReachJoltageLevels2)
|> printfn "Part 2: %d"
