open System.IO

let parse (text: string) = text.Split(',') |> Seq.map int |> Seq.toList

let getTotalFuelWithConstantRate positions position =
    positions
    |> List.sumBy (fun v -> abs (v - position))

let findOptimalPositionWithConstantFuelRate positions =
    [List.min positions .. List.max positions]
    |> List.map (getTotalFuelWithConstantRate positions)
    |> List.min

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> findOptimalPositionWithConstantFuelRate
|> printfn "Part 1: %d"

let sum1ToN n =
    if n % 2 = 0 then n / 2 * (n + 1)
    else (n + 1) / 2 * n

let getTotalFuelWithIncreasedRate positions position =
    positions
    |> List.sumBy (fun v -> abs (v - position) |> sum1ToN)

let findOptimalPositionWithIncreasedFuelRate positions =
    [List.min positions .. List.max positions]
    |> List.map (getTotalFuelWithIncreasedRate positions)
    |> List.min

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> findOptimalPositionWithIncreasedFuelRate
|> printfn "Part 2: %d"
