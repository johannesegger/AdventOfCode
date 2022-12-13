open System.IO
open System.Diagnostics

type Item = Single of int | List of Item list

let rec parseItems idx (v: string) =
    let (endIdx, item) =
        if v.[idx] = '[' then
            let (endIdx, items) = parseItems (idx + 1) v
            Debug.Assert(v.[endIdx] = ']', "List should end with ']'")
            if endIdx + 1 < v.Length && v.[endIdx + 1] = ',' then
                let (endIdx, nextItems) = parseItems (idx + 2) v
                endIdx + 1, [ List items; yield! nextItems ]
            else
                idx + 1, [ List items ]
        else
            let next = v.IndexOfAny([|','; ']'|], idx)
            if next = -1 then v.Length, [ Single (int <| v.Substring(idx)) ]
            else
                if v.[next] = ',' then
                    let (endIdx, nextItems) = parseItems (next + 1) v
                    endIdx + 1, [ Single (int <| v.Substring(idx, next - idx)); yield! nextItems ]
                else
                    next + 1, [ Single (int <| v.Substring(idx, next - idx)) ]
    [ item; nextItems ]

let parsePair (lines: string array) =
    parseItems 0 lines.[0],
    parseItems 0 lines.[1]

File.ReadLines("sample.txt")
|> Seq.chunkBySize 3
|> Seq.map parsePair
|> Seq.iter (printfn "Part 1: %A")
