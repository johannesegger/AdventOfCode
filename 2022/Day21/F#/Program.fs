open FSharp.Core.Operators.Checked
open System.IO
open System.Text.RegularExpressions

type Operation = Add | Sub | Mul | Div
module Operation =
    let parse = function
        | "+" -> Add
        | "-" -> Sub
        | "*" -> Mul
        | "/" -> Div
        | v -> failwith $"Invalid operand \"%s{v}\""
    let calculateResult (left, right) = function
        | Add -> left + right
        | Sub -> left - right
        | Mul -> left * right
        | Div -> left / right
    let calculateLeftOperand (right, result) = function
        | Add -> result - right
        | Sub -> result + right
        | Mul -> result / right
        | Div -> result * right
    let calculateRightOperand (left, result) = function
        | Add -> result - left
        | Sub -> left - result
        | Mul -> result / left
        | Div -> left / result

type Monkey =
    | Result of int64
    | Calculation of string * Operation * string

let parseMonkey (v: string) =
    let m = Regex.Match(v, @"^(?<monkey>\w+): ((?<number>\d+)|(?<monkey1>\w+) (?<op>.) (?<monkey2>\w+))$")
    if not m.Success then failwith $"Failed to parse monkey: \"%s{v}\""
    let name = m.Groups.["monkey"].Value
    if m.Groups.["number"].Success then
        name, Result (int64 m.Groups.["number"].Value)
    else
        let operand1 = m.Groups.["monkey1"].Value
        let operation = Operation.parse m.Groups.["op"].Value
        let operand2 = m.Groups.["monkey2"].Value
        name, Calculation (operand1, operation, operand2)

let monkeys =
    File.ReadLines("input.txt")
    |> Seq.map parseMonkey
    |> Map.ofSeq

let rec evaluateCalculation = function
    | Result v -> v
    | Calculation (left, op, right) ->
        let leftValue = Map.find left monkeys |> evaluateCalculation
        let rightValue = Map.find right monkeys |> evaluateCalculation
        Operation.calculateResult (leftValue, rightValue) op

monkeys
|> Map.find "root"
|> evaluateCalculation
|> printfn "Part 1: %d"

let rec needsHumn monkey =
    if monkey = "humn" then true
    else
        match Map.find monkey monkeys with
        | Result _ -> false
        | Calculation (left, _, right) ->
            needsHumn left || needsHumn right

let rec getHumnValueForResult value monkey =
    match Map.find monkey monkeys with
    | Result _ when monkey = "humn" -> value
    | Result v -> v
    | Calculation (left, op, right) ->
        match needsHumn left, needsHumn right with
        | true, false ->
            let rightValue = Map.find right monkeys |> evaluateCalculation
            let targetValue = Operation.calculateLeftOperand (rightValue, value) op
            left |> getHumnValueForResult targetValue
        | false, true ->
            let leftValue = Map.find left monkeys |> evaluateCalculation
            let targetValue = Operation.calculateRightOperand (leftValue, value) op
            right |> getHumnValueForResult targetValue
        | true, true -> failwith $"Can't evaluate equation of %s{monkey} because both terms contain humn"
        | false, false -> failwith $"Didn't find humn in any term of %s{monkey}"

let evaluateRootEquation = function
    | Result _ -> failwith "Not a calculation"
    | Calculation (left, op, right) ->
        match needsHumn left, needsHumn right with
        | true, false ->
            let rightValue = Map.find right monkeys |> evaluateCalculation
            left |> getHumnValueForResult rightValue
        | false, true ->
            let leftValue = Map.find left monkeys |> evaluateCalculation
            right |> getHumnValueForResult leftValue
        | true, true -> failwith "Can't evaluate equation of root if both terms contain humn"
        | false, false -> failwith "Didn't find humn in any term of root"

monkeys
|> Map.find "root"
|> evaluateRootEquation
|> printfn "Part 2: %d"
