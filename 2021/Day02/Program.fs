open System.IO

type Command = Forward of int | Down of int | Up of int
let parseCommand (text: string) =
    let (name, value) =
        let parts = text.Split()
        (parts.[0], int parts.[1])
    if name = "forward" then Forward value
    elif name = "down" then Down value
    elif name = "up" then Up value
    else failwith $"Invalid command: %s{text}"

let move (x, y) = function
    | Forward v -> (x + v, y)
    | Down v -> (x, y + v)
    | Up v -> (x, y - v)

File.ReadAllLines "input.txt"
|> Seq.map parseCommand
|> Seq.fold move (0, 0)
|> fun (x, y) -> x * y
|> printfn "Part 1: %d"

let moveWithAim ((x, y), aim) = function
    | Forward v -> ((x + v, y + aim * v), aim)
    | Down v -> ((x, y), aim + v)
    | Up v -> ((x, y), aim - v)

File.ReadAllLines "input.txt"
|> Seq.map parseCommand
|> Seq.fold moveWithAim ((0, 0), 0)
|> fun ((x, y), aim) -> x * y
|> printfn "Part 2: %d"
