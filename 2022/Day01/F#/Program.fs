open System.IO

let sums =
    ([], File.ReadLines("input.txt"))
    ||> Seq.fold (fun state line ->
        if line = "" then 0 :: state // Start new elf
        else
            let calories = int line
            match state with
            | currentSum :: sums -> (currentSum + calories) :: sums
            | [] -> [ calories ]
    )

let part1 = List.max sums
printfn "Part1: %d" part1

let part2 =
    sums
    |> List.sortDescending
    |> List.take 3
    |> List.sum
printfn "Part2: %d" part2
