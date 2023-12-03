open Checked
open System
open System.IO

let parseBoard lines =
    lines
    |> Seq.map (fun (line: string) ->
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toList
    )
    |> Seq.toList

let parseGame (lines: string array) =
    let numbers = lines.[0].Split(',') |> Seq.map int |> Seq.toList

    let boards =
        lines
        |> Seq.skip 1
        |> Seq.chunkBySize 6
        |> Seq.map (Seq.skip 1 >> parseBoard)
        |> Seq.toList
        
    (numbers, boards)

let isWinningBoard drawnNumbers board =
    List.append board (List.transpose board)
    |> List.exists (fun row ->
        row |> List.except drawnNumbers |> List.isEmpty
    )

let getSumOfUnmarkedNumbers drawnNumbers board =
    board
    |> List.collect id
    |> List.except drawnNumbers
    |> List.sum

let getWinningBoards numbers boards =
    ((boards, []), numbers)
    ||> List.mapFold (fun (boards, drawnNumbers) number ->
        let drawnNumbers' = number :: drawnNumbers
        let winningBoards = boards |> List.filter (isWinningBoard drawnNumbers')
        let boards' = boards |> List.except winningBoards
        let results =
            [ for board in winningBoards -> (board, number, drawnNumbers') ]
        (results, (boards', drawnNumbers'))
    )
    |> fst
    |> List.collect id

let getWinningBoardScores numbers boards =
    getWinningBoards numbers boards
    |> List.map (fun (board, number, drawnNumbers) ->
        getSumOfUnmarkedNumbers drawnNumbers board * number
    )

let playUntilFirstBoardWins (numbers, boards) =
    getWinningBoardScores numbers boards
    |> List.tryHead
    |> Option.defaultWith (fun () -> failwith "Game over. No board won.")

File.ReadAllLines "input.txt"
|> parseGame
|> playUntilFirstBoardWins
|> printfn "Part 1: %d"

let playUntilLastBoardWins (numbers, boards) =
    getWinningBoardScores numbers boards
    |> List.tryLast
    |> Option.defaultWith (fun () -> failwith "Game over. No board won.")

File.ReadAllLines "input.txt"
|> parseGame
|> playUntilLastBoardWins
|> printfn "Part 2: %d"
