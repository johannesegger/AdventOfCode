type RegisterName = char
type RegisterValue = int

type ValueProvider =
    | Register of RegisterName
    | Constant of RegisterValue

module ValueProvider =
    let parse (text: string) =
        let (success, value) = RegisterValue.TryParse text
        if success then Constant value
        else RegisterName.Parse text |> Register

type Instruction =
    | Set of RegisterName * ValueProvider
    | Sub of RegisterName * ValueProvider
    | Mul of RegisterName * ValueProvider
    | Jnz of ValueProvider * RegisterValue

let parseLine (line: string) =
    match line.Split ' ' with
    | [| "set"; register; value |] -> Set (RegisterName.Parse register, ValueProvider.parse value)
    | [| "sub"; register; value |] -> Sub (RegisterName.Parse register, ValueProvider.parse value)
    | [| "mul"; register; value |] -> Mul (RegisterName.Parse register, ValueProvider.parse value)
    | [| "jnz"; condition; offset |] -> Jnz (ValueProvider.parse condition, RegisterValue.Parse offset)
    | _ -> failwithf "Can't parse line: '%s'" line

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day23-input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let initialRegisters =
    [for c in 'a'..'h' -> c, 0]
    |> Map.ofList

type Registers = Map<RegisterName, RegisterValue>

type Position = int

type Data = Position * Registers

let getValue registers = function
    | Register name -> Map.find name registers
    | Constant value -> value

let updateRegister registers register fn =
    let currentValue = getValue registers (Register register)
    let newValue = fn currentValue
    Map.add register newValue registers

let execute (position, registers) =
    let getValue = getValue registers
    match input.[position] with
    | Set (register, value) ->
        position + 1,
        updateRegister registers register (fun _ -> getValue value)
    | Sub (register, value) ->
        position + 1,
        updateRegister registers register (fun v -> v - getValue value)
    | Mul (register, value) ->
        position + 1,
        updateRegister registers register (fun v -> v * getValue value)
    | Jnz (condition, offset) ->
        let position =
            if getValue condition <> 0
            then position + offset
            else position + 1
        position, registers

let isProgramEnd position =
    List.tryItem position input
    |> Option.isNone

let isMultiplication position =
    match List.item position input with
    | Mul _ -> true
    | _ -> false

let solution1 =
    let execute' (mulCount, (position, registers)) =
        let mulCount' =
            if isMultiplication position then mulCount + 1
            else mulCount
        mulCount',
        execute (position, registers)

    Seq.initInfinite ignore
    |> Seq.scan (fun p () -> execute' p) (0, (0, initialRegisters))
    |> Seq.find (snd >> fst >> isProgramEnd)
    |> fst

let solution2 =
    let isPrime v =
        [3..v/2]
        |> List.append [2]
        |> List.forall (fun n -> v % n <> 0)

    [108100..17..125100]
    |> List.filter (isPrime >> not)
    |> List.length
