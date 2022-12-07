open System.IO
open System.Text.RegularExpressions

type File = {
    Name: string
    Size: int
}

type State = {
    CurrentDir: string
    Folders: Map<string, File list>
}

let rec parseFileSystem state lines =
    match lines with
    | [] -> state.Folders
    | command :: xs ->
        match command with
        | "$ cd /" ->
            let state' = { state with CurrentDir = "" }
            parseFileSystem state' xs
        | "$ cd .." ->
            let state' = { state with CurrentDir = state.CurrentDir.Substring(0, state.CurrentDir.LastIndexOf('/')) }
            parseFileSystem state' xs
        | x when x.StartsWith("$ cd ") ->
            let dirName = x.Substring("$ cd ".Length)
            let state' = { state with CurrentDir = $"{state.CurrentDir}/{dirName}" }
            parseFileSystem state' xs
        | "$ ls" ->
            let content = 
                xs
                |> List.takeWhile(fun v -> not <| v.StartsWith("$ "))
            let files =
                content
                |> List.choose (fun v ->
                    let m = Regex.Match(v, @"^(\d+) (.*)$")
                    if m.Success then Some { Name = m.Groups.[2].Value; Size = int m.Groups.[1].Value }
                    else None
                )
            let state' = { state with Folders = Map.add state.CurrentDir files state.Folders }
            let xs' =
                xs |> List.skip content.Length
            parseFileSystem state' xs'
        | x -> failwith $"Invalid command: \"{x}\""

let folders =
    File.ReadLines("input.txt")
    |> Seq.toList
    |> parseFileSystem { CurrentDir = "/"; Folders = Map.empty }

let directories =
    folders |> Seq.map (fun v -> v.Key) |> Seq.toList

let getTotalFolderSize (path: string) =
    folders
    |> Seq.filter (fun v -> v.Key.StartsWith path)
    |> Seq.sumBy (fun v -> v.Value |> List.sumBy (fun v -> v.Size))

directories
|> List.map getTotalFolderSize
|> Seq.filter (fun v -> v < 100000)
|> Seq.sum
|> printfn "Part 1: %d"

let totalUsedSpace = getTotalFolderSize ""
let totalSpace = 70_000_000
let spaceToFree = 30_000_000 - (totalSpace - totalUsedSpace)
printfn "%d" spaceToFree
directories
|> List.map getTotalFolderSize
|> Seq.filter (fun v -> v >= spaceToFree)
|> Seq.min
|> printfn "Part 2: %d"
