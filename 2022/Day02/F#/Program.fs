open System.IO

let lines = File.ReadAllLines("input.txt")

let getScore = function
    | "A X" -> 1 + 3
    | "A Y" -> 2 + 6
    | "A Z" -> 3 + 0
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 1 + 6
    | "C Y" -> 2 + 0
    | "C Z" -> 3 + 3
    | _ -> failwith "Invalid input"

lines
|> Seq.map getScore
|> Seq.sum
|> printfn "Part 1: %d"

let getGame = function
    | "A X" -> "A Z"
    | "A Y" -> "A X"
    | "A Z" -> "A Y"
    | "B X" -> "B X"
    | "B Y" -> "B Y"
    | "B Z" -> "B Z"
    | "C X" -> "C Y"
    | "C Y" -> "C Z"
    | "C Z" -> "C X"
    | _ -> failwith "Invalid input"

lines
|> Seq.map getGame
|> Seq.map getScore
|> Seq.sum
|> printfn "Part 2: %d"