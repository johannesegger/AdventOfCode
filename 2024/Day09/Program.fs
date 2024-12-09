open Checked
open System.IO

type Entry = File of id: int * size: int | Free of size: int

let parse line =
    line
    |> Seq.mapi (fun i v -> if i % 2 = 0 then File (i / 2, int (v - '0')) else Free (int (v - '0')))
    |> Seq.toList

let getFileFromEnd disk maxSize =
    let rec fn disk =
        match disk with
        | [] -> failwith "No block left"
        | File (fileId, size) :: disk ->
            if size <= maxSize then
                (List.rev disk, fileId, size)
            else
                let disk' = File (fileId, size - maxSize) :: disk
                (List.rev disk', fileId, maxSize)
        | Free _ :: disk -> fn disk
    fn (List.rev disk)

let fillFreeSpace disk =
    let rec fn disk acc =
        match disk with
        | [] -> acc
        | File _ as f :: disk' -> fn disk' (acc @ [ f ])
        | Free size :: disk' ->
            let (disk', fileId, size') = getFileFromEnd disk' size
            if size' < size then
                fn (Free (size - size') :: disk') (acc @ [ File (fileId, size') ])
            else
                fn disk' (acc @ [ File (fileId, size') ])
    fn disk []

let getChecksum disk =
    let rec fn disk position checksum =
        match disk with
        | [] -> checksum
        | File (fileId, size) :: disk' ->
            let checksum' = checksum + List.sum [ for p in position..(position + size - 1) -> int64 (fileId * p) ]
            fn disk' (position + size) checksum'
        | Free size :: disk' -> fn disk' (position + size) checksum

    fn disk 0 0

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> fillFreeSpace
|> getChecksum
|> printfn "Part 1: %d"

let moveToFirstFreeSpace disk (fileId, fileSize) =
    let rec fn disk acc =
        match disk with
        | [] -> None
        | File _ as f :: disk' -> fn disk' (f :: acc)
        | Free size as f :: disk' when size < fileSize -> fn disk' (f :: acc)
        | Free size :: disk' ->
            Some [
                yield! List.rev acc
                File (fileId, fileSize)
                if size > fileSize then Free (size - fileSize)
                yield! disk'
                Free fileSize
            ]

    fn disk []

let fillFreeSpace' disk =
    let rec fn disk acc =
        match disk with
        | [] -> acc
        | File (fileId, size) as f :: disk' ->
            match moveToFirstFreeSpace (List.rev disk') (fileId, size) with
            | Some disk' -> fn (List.rev disk') acc
            | None -> fn disk' (f :: acc)
        | Free _ as f :: disk' -> fn disk' (f :: acc)
    fn (List.rev disk) []

File.ReadAllLines "input.txt"
|> Seq.head
|> parse
|> fillFreeSpace'
|> getChecksum
|> printfn "Part 2: %d"
