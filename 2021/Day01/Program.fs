open System.IO

File.ReadAllLines "input.txt"
|> Seq.map int
|> Seq.windowed 2
|> Seq.sumBy (fun v -> if v.[0] < v.[1] then 1 else 0)
|> printfn "Part 1: %d"

File.ReadAllLines "input.txt"
|> Seq.map int
|> Seq.windowed 3
|> Seq.map Seq.sum
|> Seq.windowed 2
|> Seq.sumBy (fun v -> if v.[0] < v.[1] then 1 else 0)
|> printfn "Part 2: %d"
