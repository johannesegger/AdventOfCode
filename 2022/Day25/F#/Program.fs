open System.IO

let decodeChar = function
    | '2' -> 2
    | '1' -> 1
    | '0' -> 0
    | '-' -> -1
    | '=' -> -2
    | c -> failwith $"Invalid digit: \"%c{c}\""

let decode (v: string) =
    (0L, v)
    ||> Seq.fold (fun n c -> 5L * n + int64 (decodeChar c))

let encodeChar = function
    | 2 -> '2'
    | 1 -> '1'
    | 0 -> '0'
    | -1 -> '-'
    | -2 -> '='
    | v -> failwith $"Invalid digit: \"%d{v}\""

let encode n =
    let rec fn n acc =
        if n = 0L then acc
        else
            let rest =
                let r = n % 5L |> int
                if r < 3 then r else r - 5
            let c = encodeChar rest
            let n' = n / 5L + (if rest < 0 then 1L else 0)
            let acc' = sprintf "%c%s" c acc
            fn n' acc'
    fn n ""

File.ReadLines("input.txt")
|> Seq.sumBy decode
|> encode
|> printfn "Part 1: %A"
