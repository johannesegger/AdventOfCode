type InstructionOperator =
    | Inc
    | Dec

type Operation = {
    RegisterName: string
    Operator: InstructionOperator
    Value: int
}

type ConditionOperator =
    | GreaterThan
    | GreaterThanOrEqualTo
    | EqualTo
    | LessThanOrEqualTo
    | LessThan
    | NotEqualTo


type Condition = {
    RegisterName: string
    Operator: ConditionOperator
    Value: int
}

type Instruction = {
    Operation: Operation
    Condition: Condition
}

let parseLine line =
    let pattern = @"^(?<name>\w+)\s+(?<operation>inc|dec)\s+(?<value>-?\d+)\s+if\s+(?<conditionName>\w+)\s+(?<conditionOperator>>|>=|==|<=|<|!=)\s+(?<conditionValue>-?\d+)$"
    let m = System.Text.RegularExpressions.Regex.Match(line, pattern)
    if not m.Success then failwithf "Couldn't parse line: %s" line
    {
        Operation =
            {
                RegisterName = m.Groups.["name"].Value
                Operator =
                    if m.Groups.["operation"].Value.Equals("inc", System.StringComparison.OrdinalIgnoreCase)
                    then Inc
                    else Dec
                Value = int m.Groups.["value"].Value
            }
        Condition =
            {
                RegisterName = m.Groups.["conditionName"].Value
                Operator =
                    match m.Groups.["conditionOperator"].Value.ToLower() with
                    | ">" -> GreaterThan
                    | ">=" -> GreaterThanOrEqualTo
                    | "==" -> EqualTo
                    | "<=" -> LessThanOrEqualTo
                    | "<" -> LessThan
                    | "!=" -> NotEqualTo
                    | v -> failwithf "Unexpected conditional operator: '%s'" v
                Value = int m.Groups.["conditionValue"].Value
            }
    }

let satisfiesCondition condition registers =
    let registerValue =
        Map.tryFind condition.RegisterName registers
        |> Option.defaultValue 0

    match condition.Operator with
    | GreaterThan -> registerValue > condition.Value
    | GreaterThanOrEqualTo -> registerValue >= condition.Value
    | EqualTo -> registerValue = condition.Value
    | LessThanOrEqualTo -> registerValue <= condition.Value
    | LessThan -> registerValue < condition.Value
    | NotEqualTo -> registerValue <> condition.Value

let applyOperation (operation: Operation) registers =
    let registerValue =
        Map.tryFind operation.RegisterName registers
        |> Option.defaultValue 0
    let newValue =
        match operation.Operator with
        | Inc -> registerValue + operation.Value
        | Dec -> registerValue - operation.Value
    Map.add operation.RegisterName newValue registers

let folder registers instruction =
    if satisfiesCondition instruction.Condition registers
    then applyOperation instruction.Operation registers
    else registers

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day8-input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let solution1 =
    input
    |> Seq.fold folder Map.empty
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.max

let solution2 =
    let folder' (registers, highestValue) instruction =
        let registers = folder registers instruction
        let highestValue =
            match Map.tryFind instruction.Operation.RegisterName registers with
            | Some v when v > highestValue -> v
            | _ -> highestValue
        registers, highestValue
    input
    |> Seq.fold folder' (Map.empty, 0)
    |> snd
