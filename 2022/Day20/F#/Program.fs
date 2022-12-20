open System.IO
open FSharp.Core.Operators.Checked

type ReferencedNumber(value: int64) =
    member _.Value with get() = value

let coords =
    File.ReadLines("input.txt")
    |> Seq.map (int >> ReferencedNumber)
    |> Seq.toArray

let positiveModulo a b = (a % b + b) % b

let mix count (coords: ReferencedNumber array) =
    let move state index =
        let item = coords.[index]
        let itemIndex = Array.findIndex ((=) item) state
        let length = coords.Length - 1
        let insertIndex = positiveModulo (int64 itemIndex + item.Value) (int64 length) |> int
        state
        |> Array.removeAt itemIndex
        |> Array.insertAt insertIndex item
    let mixOnce state =
        (state, [0..coords.Length - 1])
        ||> List.fold move

    let mixed =
        (coords, [1..count])
        ||> List.fold (fun state _ -> mixOnce state)

    let startIndex = mixed |> Array.findIndex (fun v -> v.Value = 0)
    mixed.[(startIndex + 1000) % mixed.Length].Value +
    mixed.[(startIndex + 2000) % mixed.Length].Value +
    mixed.[(startIndex + 3000) % mixed.Length].Value

coords
|> mix 1
|> printfn "Part 1: %d"

coords
|> Array.map (fun v -> ReferencedNumber(v.Value * 811589153L))
|> mix 10
|> printfn "Part 2: %d"
