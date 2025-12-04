open Checked
open System.IO

let parseMap lines =
    let map =
        lines
        |> Seq.map (fun line ->
            line
            |> Seq.map ((=) '@')
            |> Seq.toArray
        )
        |> Seq.toArray
    Array2D.init map.Length map.[0].Length (fun row col -> map.[row].[col])

let getNeighborPositions (row, col) =
    [
        (row - 1, col - 1); (row - 1, col); (row - 1, col + 1)
        (row, col - 1); (row, col + 1)
        (row + 1, col - 1); (row + 1, col); (row + 1, col + 1)
    ]

let tryGet (row, col) (map: bool[,]) =
    if row >= 0 && row < map.GetLength 0 && col >= 0 && col < map.GetLength 1 then
        Some map.[row, col]
    else None

let getAccessiblePaperRolls (map: bool[,]) =
    [
        for row in 0..map.GetLength 0 - 1 do
        for col in 0..map.GetLength 1 - 1 do (row, col)
    ]
    |> List.filter (fun (row, col) ->
        let isPaperRoll = map.[row, col]
        let neighborRolls =
            getNeighborPositions (row, col)
            |> List.choose (fun p -> tryGet p map)
            |> List.filter id
            |> List.length
        isPaperRoll && neighborRolls < 4
    )

File.ReadAllLines "input.txt"
|> parseMap
|> getAccessiblePaperRolls
|> List.length
|> printfn "Part 1: %d"

let getRemovablePaperRolls map =
    let map = Array2D.copy map
    let rec fn acc =
        let accessiblePaperRolls = getAccessiblePaperRolls map
        if List.length accessiblePaperRolls = 0 then acc
        else
            accessiblePaperRolls
            |> List.iter (fun (row, col) ->
                Array2D.set map row col false
            )
            fn (acc + accessiblePaperRolls.Length)
    fn 0

File.ReadAllLines "input.txt"
|> parseMap
|> getRemovablePaperRolls
|> printfn "Part 2: %d"
