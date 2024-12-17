open Checked
open System.IO

type Operand = Literal of int | RegisterA | RegisterB | RegisterC
let getOperandValue (registerA, registerB, registerC) instruction operand =
    match instruction with
    | 1 | 3 | 4 -> operand
    | _ ->
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> registerA
        | 5 -> registerB
        | 6 -> registerC
        | 7 -> failwith $"Reserved combo operand for instruction %d{instruction}"
        | v -> failwith $"Unknown operand: %d{v}"

let execute (registerA, registerB, registerC) instruction operand =
    match instruction with
    | 0 -> ((registerA / (int (2. ** float operand)), registerB, registerC), None, None)
    | 1 -> ((registerA, registerB ^^^ operand, registerC), None, None)
    | 2 -> ((registerA, operand % 8, registerC), None, None)
    | 3 -> if registerA = 0 then ((registerA, registerB, registerC), None, None) else ((registerA, registerB, registerC), Some (operand / 2), None)
    | 4 -> ((registerA, registerB ^^^ registerC, registerC), None, None)
    | 5 -> ((registerA, registerB, registerC), None, Some (operand % 8))
    | 6 -> ((registerA, registerA / (int (2. ** float operand)), registerC), None, None)
    | 7 -> ((registerA, registerB, registerA / (int (2. ** float operand))), None, None)
    | v -> failwith $"Invalid instruction: %d{v}"

let parse (lines: string[]) =
    let regA = int lines.[0].["Register A: ".Length..]
    let regB = int lines.[1].["Register B: ".Length..]
    let regC = int lines.[2].["Register C: ".Length..]
    let program =
        lines.[4].["Program: ".Length..].Split(',')
        |> Seq.map int
        |> Seq.chunkBySize 2
        |> Seq.map (fun c -> (int c.[0], int c.[1]))
        |> Seq.toArray
    ((regA, regB, regC), program)

let run (registers, program) =
    let rec fn (registers, ptr) acc =
        match program |> Array.tryItem (ptr / 2) with
        | Some (instruction, operand) ->
            let (registers', newPtr, output) =
                getOperandValue registers instruction operand
                |> execute registers instruction
            let ptr' = newPtr |> Option.defaultValue (ptr + 2)
            let acc' = match output with | Some v -> v :: acc | None -> acc
            fn (registers', ptr') acc'
        | None -> List.rev acc
    fn (registers, 0) []

File.ReadAllLines "input.txt"
|> parse
|> run
|> List.map string
|> String.concat ","
|> printfn "Part 1: %s"

let list = [2; 4; 1; 1; 7; 5; 4; 7; 1; 4; 0; 3; 5; 5; 3; 0]
let rec fn a out =
    let b = a % 8L
    let b = b ^^^ 1L
    let c = a / int64 (2.**float b)
    let b = b ^^^ c
    let b = b ^^^ 4
    let a = a / int64 (2.**3.)
    let out' = int (b % 8L) :: out
    if a = 0 then List.rev out'
    else fn a out'

[1..list.Length]
|> List.fold (fun a n ->
    Seq.initInfinite (fun i -> a * 8L + int64 i)
    |> Seq.find (fun a -> fn a [] = list.[list.Length - n..] )
) 0L
|> printfn "Part 2: %d"
