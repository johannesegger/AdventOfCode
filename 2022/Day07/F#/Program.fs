open System.IO

let lines = File.ReadAllLines("input.txt")

let rec parseCommands folders path lineIndex =
    match lines |> Array.tryItem lineIndex with
    | None -> folders
    | Some "$ cd /" -> parseCommands folders "" (lineIndex + 1)
    | Some "$ cd .." ->
        let newPath = path.Substring(0, path.LastIndexOf('/'))
        parseCommands folders newPath (lineIndex + 1)
    | Some x when x.StartsWith("$ cd ") ->
        let newPath = $"{path}/{x.Substring(5)}"
        parseCommands folders newPath (lineIndex + 1)
    | Some "$ ls" ->
        let contentLines =
            lines
            |> Array.skip (lineIndex + 1)
            |> Array.takeWhile (fun line -> line.[0] <> '$')
        let size =
            contentLines
            |> Array.filter (fun line -> not <| line.StartsWith("dir"))
            |> Array.sumBy (fun line -> line.Substring(0, line.IndexOf(' ')) |> int)
        let newState = Map.add path size folders
        parseCommands newState path (lineIndex + 1 + contentLines.Length)
    | x -> failwith $"Invalid command: \"{x}\""
let folders = parseCommands Map.empty "" 0

let directories =
    folders |> Seq.map (fun v -> v.Key) |> Seq.toList

let getTotalFolderSize (path: string) =
    folders
    |> Seq.filter (fun v -> v.Key.StartsWith path)
    |> Seq.sumBy (fun v -> v.Value)

directories
|> List.map getTotalFolderSize
|> Seq.filter (fun v -> v < 100000)
|> Seq.sum
|> printfn "Part 1: %d"

let totalUsedSpace = getTotalFolderSize ""
let totalSpace = 70_000_000
let spaceToFree = 30_000_000 - (totalSpace - totalUsedSpace)

directories
|> List.map getTotalFolderSize
|> Seq.filter (fun v -> v >= spaceToFree)
|> Seq.min
|> printfn "Part 2: %d"
