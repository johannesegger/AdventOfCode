open System.IO

let parseEnergyLevels lines =
    lines |> Array.map (Seq.map (fun c -> int (c - '0')) >> Seq.toArray)

let mapEnergyLevel fn energyLevels =
    energyLevels
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun colIndex v -> fn (rowIndex, colIndex) v)
    )

let increaseAll energyLevels =
    energyLevels |> mapEnergyLevel (fun _ v -> v + 1)

let getFlashCount energyLevels =
    energyLevels
    |> Array.sumBy (Seq.filter (fun v -> v > 9) >> Seq.length)

let getNeighbors energyLevels (row, col) =
    [
        (row - 1, col - 1)
        (row - 1, col)
        (row - 1, col + 1)
        (row, col - 1)
        (row, col + 1)
        (row + 1, col - 1)
        (row + 1, col)
        (row + 1, col + 1)
    ]
    |> List.choose (fun (row, col) ->
        energyLevels
        |> Array.tryItem row
        |> Option.bind (Array.tryItem col)
    )

let getFlashingNeighborCount energyLevels (row, col) =
    getNeighbors energyLevels (row, col)
    |> List.filter (fun v -> v > 9)
    |> List.length

let processFlashes energyLevels =
    let rec fn energyLevels totalFlashCount =
        let afterFlashEnergyLevels =
            energyLevels
            |> mapEnergyLevel (fun index v ->
                if v < 0 || v > 9 then -1 // ensure flash is only processed once
                else v + (getFlashingNeighborCount energyLevels index)
            )
        let countFlashes = getFlashCount afterFlashEnergyLevels
        if countFlashes = 0 then
            let resetEnergyLevels =
                afterFlashEnergyLevels
                |> mapEnergyLevel (fun _ v -> if v < 0 then 0 else v)
            (resetEnergyLevels, totalFlashCount)
        else
            fn afterFlashEnergyLevels (totalFlashCount + countFlashes)
    fn energyLevels (getFlashCount energyLevels)

let run steps energyLevels =
    let rec fn steps energyLevels flashCount =
        if steps = 0 then flashCount
        else
            let (energyLevels', newFlashCount) =
                energyLevels |> increaseAll |> processFlashes
            fn (steps - 1) energyLevels' (flashCount + newFlashCount)
    fn steps energyLevels 0

File.ReadAllLines "input.txt"
|> parseEnergyLevels
|> run 100
|> printfn "Part 1: %d"

let allFlash = Array.forall (Array.forall ((=) 0))

let runUntilAllFlash energyLevels =
    let rec fn energyLevels steps =
        if allFlash energyLevels then steps
        else
            let energyLevels' =
                energyLevels |> increaseAll |> processFlashes |> fst
            fn energyLevels' (steps + 1)
    fn energyLevels 0

File.ReadAllLines "input.txt"
|> parseEnergyLevels
|> runUntilAllFlash
|> printfn "Part 2: %d"
