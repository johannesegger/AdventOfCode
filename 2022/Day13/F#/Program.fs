open System
open System.IO
open System.Diagnostics

type Item = Single of int | List of Item list

let parseInt startIndex (v: string) =
    let endIndex =
        match v.IndexOfAny([| ','; ']' |], startIndex) with
        | -1 -> v.Length
        | x -> x
    (endIndex, int (v.Substring(startIndex, endIndex - startIndex)))

let rec parseListItems idx (v: string) acc =
    if v.[idx] = ']' then (idx + 1, acc)
    elif v.[idx] = ',' then parseListItems (idx + 1) v acc // TODO supports ,]
    elif Char.IsDigit v.[idx] then
        let (nextIdx, number) = parseInt idx v
        parseListItems nextIdx v (acc @ [ Single number ])
    elif v.[idx] = '[' then
        let (nextIdx, items) = parseListItems (idx + 1) v []
        parseListItems nextIdx v (acc @ [ List items ])
    else failwith $"Unexpected char \"%c{v.[idx]}\" at index %d{idx} in line \"%s{v}\""

let rec parseList idx (v: string) =
    let (endIdx, items) = parseListItems idx v []
    Debug.Assert((endIdx = v.Length), "Cursor should be at end")
    List items

let rec parseItems (v: string) =
    Debug.Assert(v.[0] = '[', $"Expected '[' at index %d{0}")
    parseList 1 v

type CompareResult = Lt | Eq | Gt
module CompareResult =
    let toInt = function
        | Lt -> -1
        | Eq -> 0
        | Gt -> 1

let compareInt a b =
    if a < b then Lt
    elif a > b then Gt
    else Eq

let rec compareItems a b =
    match a, b with
    | Single x, Single y -> compareInt x y
    | Single _, List _ -> compareItems (List [a]) b
    | List _, Single _ -> compareItems a (List [b])
    | List x, List y ->
        Seq.zip x y
        |> Seq.map (fun (v1, v2) -> compareItems v1 v2)
        |> Seq.tryFind (fun v -> v <> Eq)
        |> Option.defaultValue (compareInt x.Length y.Length)

let packets =
    File.ReadLines("input.txt")
    |> Seq.filter (fun line -> line.Length > 0)
    |> Seq.map parseItems
    |> Seq.toList
packets
|> Seq.chunkBySize 2
|> Seq.mapi (fun i chunk ->
    if compareItems chunk.[0] chunk.[1] = Lt then i + 1 else 0
)
|> Seq.sum
|> printfn "Part 1: %d"

let dividerPackets = [
    List [ List [Single 2] ]
    List [ List [Single 6] ]
]

packets
|> List.append dividerPackets
|> List.sortWith (fun a b -> compareItems a b |> CompareResult.toInt)
|> fun x ->
    dividerPackets
    |> List.map (fun p -> List.findIndex ((=)p) x + 1)
    |> List.reduce ((*))
|> printfn "Part 2: %d"
