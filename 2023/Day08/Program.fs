open Checked
open System.IO
open System.Text.RegularExpressions

let parseInstruction text =
    let m = Regex.Match(text, @"^(\w+) = \((\w+), (\w+)\)$")
    (m.Groups.[1].Value, (m.Groups.[2].Value, m.Groups.[3].Value))

let parseDirection = function
    | 'R' -> snd
    | 'L' -> fst
    | v -> failwith $"Invalid direction: %c{v}"

let parse (lines: string array) =
    let directions = lines.[0] |> Seq.map parseDirection |> Seq.toArray
    let map = lines |> Seq.skip 2 |> Seq.map parseInstruction |> Map.ofSeq
    (directions, map)

let run ((directions: _ array), map) =
    let rec fn position directionIndex steps =
        if position = "ZZZ" then steps
        else
            let direction = directions.[directionIndex]
            let position' =
                Map.tryFind position map
                |> Option.defaultWith (fun () -> failwith $"Can't find position %s{position} in map")
                |> direction
            fn position' ((directionIndex + 1) % directions.Length) (steps + 1)
    
    fn "AAA" 0 0

File.ReadAllLines "input.txt"
|> parse
|> run
|> printfn "Part 1: %d"

let lcm a b =
    let rec gcd a b =
        if b = 0L then a
        else gcd b (a % b)
    a * b / gcd a b

let runAsGhost (directions: _ array, map) =
    let rec fn (position: string) directionIndex steps =
        if position.EndsWith "Z" then steps
        else
            let direction = directions.[directionIndex]
            let position' =
                Map.tryFind position map
                |> Option.defaultWith (fun () -> failwith $"Can't find position %s{position} in map")
                |> direction
            fn position' ((directionIndex + 1) % directions.Length) (steps + 1)
    
    Map.keys map
    |> Seq.filter (fun v -> v.EndsWith "A")
    |> Seq.map (fun p -> fn p 0 0)
    |> Seq.map int64
    |> Seq.reduce lcm

File.ReadAllLines "input.txt"
|> parse
|> runAsGhost
|> printfn "Part 2: %d"
