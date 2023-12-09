open Checked
open System.IO

type ChunkType = Parentheses | SquareBrackets | Braces | AngleBrackets
type Chunk = OpenChunk of ChunkType | CloseChunk of ChunkType

let parseChunk = function
    | '(' -> OpenChunk Parentheses
    | ')' -> CloseChunk Parentheses
    | '[' -> OpenChunk SquareBrackets
    | ']' -> CloseChunk SquareBrackets
    | '{' -> OpenChunk Braces
    | '}' -> CloseChunk Braces
    | '<' -> OpenChunk AngleBrackets
    | '>' -> CloseChunk AngleBrackets
    | c -> failwith $"Illegal character: %c{c}"

let tryGetIllegalClosingCharacter line =
    let rec fn line openChunks =
        match line, openChunks with
        | [], _ -> (None, openChunks)
        | OpenChunk chunkType :: remainingCharacters, _ ->
            fn remainingCharacters (chunkType :: openChunks)
        | CloseChunk closeChunkType :: remainingCharacters, openChunkType :: remainingOpenChunks when closeChunkType = openChunkType ->
            fn remainingCharacters remainingOpenChunks
        | CloseChunk closeChunkType :: _, _ ->
            (Some closeChunkType, openChunks)
    fn line []

let getErrorScore line =
    match tryGetIllegalClosingCharacter line with
    | Some Parentheses, _ -> 3
    | Some SquareBrackets, _ -> 57
    | Some Braces, _ -> 1197
    | Some AngleBrackets, _ -> 25137
    | None, _ -> 0

File.ReadAllLines "input.txt"
|> Seq.map (Seq.map parseChunk >> Seq.toList)
|> Seq.sumBy getErrorScore
|> printfn "Part 1: %d"

let getAutoCompleteScore = function
    | Parentheses -> 1L
    | SquareBrackets -> 2L
    | Braces -> 3L
    | AngleBrackets -> 4L

let tryGetAutoCompleteScore line =
    match tryGetIllegalClosingCharacter line with
    | Some _, _ -> None
    | None, openChunks ->
        (0L, openChunks)
        ||> List.fold (fun score chunkType ->
            score * 5L + getAutoCompleteScore chunkType
        )
        |> Some

File.ReadAllLines "input.txt"
|> Seq.map (Seq.map parseChunk >> Seq.toList)
|> Seq.choose tryGetAutoCompleteScore
|> Seq.sort
|> Seq.toArray
|> fun scores -> scores.[scores.Length / 2]
|> printfn "Part 2: %d"
