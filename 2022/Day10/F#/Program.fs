open System
open System.IO

let cycle ((x, n), xs) line =
    match line with
    | "noop" -> ((x, n + 1), (x, n) :: xs)
    | v when v.StartsWith("addx ") ->
        let d = v.Substring("addx ".Length) |> int
        ((x + d, n + 2), (x, n + 1) :: (x, n) :: xs)
    | v -> failwith $"Invalid command \"{v}\""

let states =
    (((1, 1), []), File.ReadAllLines("input.txt"))
    ||> Seq.fold cycle
    |> fun (x, xs) ->
        x :: xs
        |> List.rev
        |> List.toArray

let getStateAtCycle number =
    fst states.[number - 1]

Seq.init 6 (fun x -> x * 40 + 20)
|> Seq.sumBy (fun v -> getStateAtCycle v * v)
|> printfn "Part 1: %d"

printf "Part 2: "
for row in [0..5] do
    if row > 0 then printf "        "
    for column in [0..39] do
        let cycle = row * 40 + column
        let spritePosition = getStateAtCycle (cycle + 1)
        if Math.Abs (spritePosition - cycle % 40) <= 1 then printf "#"
        else printf "."
    printfn ""
