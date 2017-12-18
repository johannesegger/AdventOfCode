type Register = char
type RegisterValue = int64

type ValueProvider =
    | Constant of RegisterValue
    | Register of Register

type Instruction =
    | Snd of ValueProvider
    | Assign of Register * ValueProvider
    | Add of Register * ValueProvider
    | Multiply of Register * ValueProvider
    | Modulo of Register * ValueProvider
    | Rcv of Register
    | JumpIfGreaterThanZero of ValueProvider * ValueProvider

let toValueProvider value =
    let (success, intValue) = System.Int64.TryParse value
    if success then Constant intValue
    else char value |> Register

let parseLine (line: string) =
    match line.Split ' ' |> Array.toList with
    | "snd" :: [ value ] -> Snd (toValueProvider value)
    | "set" :: variable :: [ value ] -> Assign (char variable, toValueProvider value)
    | "add" :: variable :: [ value ] -> Add (char variable, toValueProvider value)
    | "mul" :: variable :: [ value ] -> Multiply (char variable, toValueProvider value)
    | "mod" :: variable :: [ value ] -> Modulo (char variable, toValueProvider value)
    | "rcv" :: [ variable ] -> Rcv (char variable)
    | "jgz" :: condition :: [ offset ] -> JumpIfGreaterThanZero (toValueProvider condition, toValueProvider offset)
    | _ -> failwithf "Can't parse instruction: '%s'" line

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day18-input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let getValue registers = function
    | Constant v -> v
    | Register r -> Map.tryFind r registers |> Option.defaultValue 0L

let modifyRegister registers register value operation =
    let currentValue = Map.tryFind register registers |> Option.defaultValue 0L
    Map.add register (operation currentValue (getValue registers value)) registers

type Interpreter = {
    Position: int
    Registers: Map<Register, RegisterValue>
}

let defaultInterpreter = {
    Position = 0
    Registers = Map.empty
}

let advancePosition interpreter =
    { interpreter with Position = interpreter.Position + 1 }

let getInstruction interpreter =
    List.item interpreter.Position input

let interpret interpreter =
    let getValue = getValue interpreter.Registers

    match getInstruction interpreter with
    | Snd _ -> advancePosition interpreter
    | Assign (register, value) ->
        { interpreter with
            Registers = Map.add register (getValue value) interpreter.Registers
        }
        |> advancePosition
    | Add (register, value) ->
        { interpreter with
            Registers = modifyRegister interpreter.Registers register value (+)
        }
        |> advancePosition
    | Multiply (register, value) ->
        { interpreter with
            Registers = modifyRegister interpreter.Registers register value (*)
        }
        |> advancePosition
    | Modulo (register, value) ->
        { interpreter with
            Registers = modifyRegister interpreter.Registers register value (%)
        }
        |> advancePosition
    | Rcv _ -> advancePosition interpreter
    | JumpIfGreaterThanZero (condition, offset) ->
        if getValue condition > 0L
        then
            { interpreter with
                Position = interpreter.Position + (getValue offset |> int)
            }
        else
            advancePosition interpreter

module Solution1 =
    type Program = {
        Interpreter: Interpreter
        MostRecentlyPlayedSound: RegisterValue option
        MostRecentlyRecoveredSound: RegisterValue option
    }

    let startProgram = {
        Interpreter = defaultInterpreter
        MostRecentlyPlayedSound = None
        MostRecentlyRecoveredSound = None
    }

    let folder program =
        let getValue = getValue program.Interpreter.Registers
        
        let program' = { program with Interpreter = interpret program.Interpreter }
        match getInstruction program.Interpreter with
        | Snd v ->
            { program' with
                MostRecentlyPlayedSound = getValue v |> Some
            }
        | Rcv register ->
            { program' with
                MostRecentlyRecoveredSound =
                    if getValue (Register register) <> 0L
                    then program.MostRecentlyPlayedSound
                    else program.MostRecentlyRecoveredSound
            }
        | _ -> program'

    let solution =
        Seq.initInfinite ignore
        |> Seq.scan (fun interpreter () -> folder interpreter) startProgram
        |> Seq.choose (fun interpreter -> interpreter.MostRecentlyRecoveredSound)
        |> Seq.head

module Solution2 =
    type Endpoint = {
        Interpreter: Interpreter
        SendBuffer: RegisterValue list
        ReceiveBuffer: RegisterValue list
        SentValues: int
    }

    type Endpoints = {
        A: Endpoint
        B: Endpoint
    }

    let folder endpoint =
        let getValue = getValue endpoint.Interpreter.Registers
        
        let endpoint' = { endpoint with Interpreter = interpret endpoint.Interpreter }
        match getInstruction endpoint.Interpreter with
        | Snd v ->
            { endpoint' with
                SendBuffer = getValue v :: endpoint'.SendBuffer
            }
        | Rcv register ->
            let (h, t) = List.head endpoint.ReceiveBuffer, List.tail endpoint.ReceiveBuffer
            { endpoint' with
                Interpreter =
                    { endpoint'.Interpreter with
                        Registers = Map.add register h endpoint.Interpreter.Registers
                    }
                ReceiveBuffer = t
            }
        | _ -> endpoint'

    let isReceiveInstruction = function
        | Rcv _ -> true
        | _ -> false

    let needsReceiving endpoint =
        endpoint.ReceiveBuffer |> List.isEmpty
        &&
        List.item endpoint.Interpreter.Position input |> isReceiveInstruction

    let iterate endpoint =
        Seq.initInfinite ignore
        |> Seq.scan (fun interpreter () -> folder interpreter) endpoint
        |> Seq.find needsReceiving

    let syncEndpoints endpoints =
        let endpointA = iterate endpoints.A
        let endpointB = iterate endpoints.B
        {
            A = { endpointA with
                    SendBuffer = []
                    ReceiveBuffer = endpointA.ReceiveBuffer @ (List.rev endpointB.SendBuffer)
                    SentValues = endpointA.SentValues + endpointA.SendBuffer.Length }
            B = { endpointB with
                    SendBuffer = []
                    ReceiveBuffer = endpointB.ReceiveBuffer @ (List.rev endpointA.SendBuffer)
                    SentValues = endpointB.SentValues + endpointB.SendBuffer.Length }
        }

    let solution =
        let startEndpoints = {
            A = {
                Interpreter =
                    { defaultInterpreter
                        with Registers = ['p', 0L] |> Map.ofList
                    }
                ReceiveBuffer = []
                SendBuffer = []
                SentValues = 0
            }
            B = {
                Interpreter =
                    { defaultInterpreter
                        with Registers = ['p', 1L] |> Map.ofList
                    }
                ReceiveBuffer = []
                SendBuffer = []
                SentValues = 0
            }
        }
        Seq.initInfinite ignore
        |> Seq.scan (fun endpoints () -> syncEndpoints endpoints) startEndpoints
        |> Seq.find (fun endpoints -> needsReceiving endpoints.A && needsReceiving endpoints.B)
