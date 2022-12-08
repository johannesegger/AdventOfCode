open System.IO

let lines = File.ReadAllLines("input.txt")

let width = lines.[0].Length
let innerWidth = width - 2
let height = lines.Length
let innerHeight = height - 2

let getTreeHeight x y =
    int lines.[y].[x]

let isVisible (x, y) =
    let treeHeight = getTreeHeight x y
    let left = [0 .. x - 1] |> List.forall (fun x' -> getTreeHeight x' y < treeHeight)
    let right = [x + 1 .. width - 1] |> List.forall (fun x' -> getTreeHeight x' y < treeHeight)
    let top = [0 .. y - 1] |> List.forall (fun y' -> getTreeHeight x y' < treeHeight)
    let bottom = [y + 1 .. height - 1] |> List.forall (fun y' -> getTreeHeight x y' < treeHeight)
    left || right || top || bottom

let innerCoordinates =
    [
        for x in [ 1 .. innerWidth ] do
        for y in [ 1 .. innerHeight ] -> (x, y)
    ]

let countVisible =
    let border = 2 * width + 2 * height - 4
    innerCoordinates
    |> List.filter isVisible
    |> List.length
    |> fun v -> v + border

countVisible
|> printfn "Part 1: %d"

let calculateScenicScore (x, y) =
    let treeHeight = getTreeHeight x y
    let left =
        [x - 1 .. -1 .. 1]
        |> Seq.takeWhile (fun x' -> getTreeHeight x' y < treeHeight)
        |> Seq.length
        |> fun v -> v + 1
    let right =
        [x + 1 .. innerWidth]
        |> Seq.takeWhile (fun x' -> getTreeHeight x' y < treeHeight)
        |> Seq.length
        |> fun v -> v + 1
    let up =
        [y - 1 .. -1 .. 1]
        |> Seq.takeWhile (fun y' -> getTreeHeight x y' < treeHeight)
        |> Seq.length
        |> fun v -> v + 1
    let down =
        [y + 1 .. innerHeight]
        |> Seq.takeWhile (fun y' -> getTreeHeight x y' < treeHeight)
        |> Seq.length
        |> fun v -> v + 1
    left * right * up * down

innerCoordinates
|> List.map calculateScenicScore
|> List.max
|> printfn "Part 2: %d"
