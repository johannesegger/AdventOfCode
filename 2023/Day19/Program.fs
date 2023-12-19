open Checked
open System.IO
open System.Text.RegularExpressions

type Item = { X: int; M: int; A: int; S: int }

type Property = X | M | A | S
type Operator = LessThan | GreaterThan
type Condition = Property * Operator * int
type WorkflowCondition =
    | ConditionallyGoTo of Condition
    | AlwaysGoTo

let parseCondition (condition: string) =
    let m = Regex.Match(condition, @"^(\w)(<|>)(\d+)$")
    let property =
        match m.Groups.[1].Value with
        | "x" -> X
        | "m" -> M
        | "a" -> A
        | "s" -> S
        | v -> failwith $"Invalid property: %s{v}"
    let operator =
        match m.Groups.[2].Value with
        | "<" -> LessThan
        | ">" -> GreaterThan
        | v -> failwith $"Invalid operator: %s{v}"
    let value = int m.Groups.[3].Value
    (property, operator, value)

let parseWorkflowCondition (text: string) =
    let parts = text.Split(':')
    if parts.Length = 2 then ConditionallyGoTo (parseCondition parts.[0]), parts.[1]
    else AlwaysGoTo, parts.[0]

let parseWorkflow (line: string) =
    let m = Regex.Match(line, @"^(\w+)\{(.+)\}$")
    let name = m.Groups.[1].Value
    let conditions =
        m.Groups.[2].Value.Split(',')
        |> Seq.map parseWorkflowCondition
        |> Seq.toList
    (name, conditions)

let parseItem line =
    let m = Regex.Match(line, @"^{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}")
    { X = int m.Groups.[1].Value; M = int m.Groups.[2].Value; A = int m.Groups.[3].Value; S = int m.Groups.[4].Value }

let parseInput (lines: string array) =
    let workflows =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.map parseWorkflow
        |> Map.ofSeq
    let items =
        lines
        |> Seq.skipWhile ((<>) "")
        |> Seq.skip 1
        |> Seq.map parseItem
        |> Seq.toList
    (workflows, items)

let matchesCondition item (property, operator, value) =
    let propertyValue =
        match property with
        | X -> item.X
        | M -> item.M
        | A -> item.A
        | S -> item.S
    match operator with
    | LessThan -> propertyValue < value
    | GreaterThan -> propertyValue > value

let tryMatch item = function
    | ConditionallyGoTo condition, workflow -> if matchesCondition item condition then Some workflow else None
    | AlwaysGoTo, workflow -> Some workflow

let findNextWorkflow item conditions =
    conditions |> List.pick (tryMatch item)

let isAccepted workflows item =
    let rec fn workflow =
        if workflow = "A" then true
        elif workflow = "R" then false
        else
            let workflow' = Map.find workflow workflows |> findNextWorkflow item
            fn workflow'

    fn "in"

let getAcceptedParts (workflows, items) =
    items
    |> List.filter (isAccepted workflows)

let getRatingSum v =
    v.X + v.M + v.A + v.S

File.ReadAllLines "input.txt"
|> parseInput
|> getAcceptedParts
|> List.sumBy getRatingSum
|> printfn "Part 1: %d"

type ItemBounds = {
    X: int * int
    M: int * int
    A: int * int
    S: int * int
}

let getCombinations bounds =
    let getRange (lower, upper) = upper - lower + 1 |> max 0
    (getRange bounds.X |> int64) *
    (getRange bounds.M |> int64) *
    (getRange bounds.A |> int64) *
    (getRange bounds.S |> int64)

let applyBounds (bounds: ItemBounds) = function
     | ConditionallyGoTo (property, operator, value) ->
        let operatorFn =
            match operator with
            | LessThan -> (fun (lower, upper) -> (lower, value - 1 |> min upper))
            | GreaterThan -> (fun (lower, upper) -> (value + 1 |> max lower, upper))
        match property with
        | X -> { bounds with X = operatorFn bounds.X }
        | M -> { bounds with M = operatorFn bounds.M }
        | A -> { bounds with A = operatorFn bounds.A }
        | S -> { bounds with S = operatorFn bounds.S }
     | AlwaysGoTo -> bounds

let reverseCondition = function
     | ConditionallyGoTo (property, LessThan, value) -> ConditionallyGoTo (property, GreaterThan, value - 1)
     | ConditionallyGoTo (property, GreaterThan, value) -> ConditionallyGoTo (property, LessThan, value + 1)
     | AlwaysGoTo -> AlwaysGoTo // Not correct, but these are always last so it doesn't matter

let getAcceptedCombinations workflows =
    let rec fn workflow bounds =
        if workflow = "A" then getCombinations bounds
        elif workflow = "R" then 0L
        else
            let conditions = Map.find workflow workflows
            (bounds, conditions)
            ||> List.mapFold (fun bounds (condition, workflow') ->
                let finalBounds =
                    applyBounds bounds condition
                    |> fn workflow'
                let nextBounds = reverseCondition condition |> applyBounds bounds
                (finalBounds, nextBounds)
            )
            |> fst
            |> List.sum
    fn "in" { X = (1, 4000); M = (1, 4000); A = (1, 4000); S = (1, 4000) }

File.ReadAllLines "input.txt"
|> parseInput
|> fst
|> getAcceptedCombinations
|> printfn "Part 2: %d"
