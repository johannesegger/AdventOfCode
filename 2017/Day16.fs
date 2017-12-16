type Instruction =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let split (separator: char) (s: string) = s.Split separator

let parseInstruction (text: string) =
    if text.StartsWith "s" then text.Substring 1 |> int |> Spin
    elif text.StartsWith "x" then text.Substring 1 |> split '/' |> fun a -> Exchange (int a.[0], int a.[1])
    elif text.StartsWith "p" then text.Substring 1 |> split '/' |> fun a -> Partner (char a.[0], char a.[1])
    else failwithf "Can't parse instruction: '%s'" text

let input =
    System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\Day16-input.txt")
    |> split ','
    |> Seq.map parseInstruction
    |> Seq.toList

let initial = "abcdefghijklmnop"


let applyInstruction (programs: string) instruction =
    let partner a b (text: string) =
        text
            .Replace(a, '_')
            .Replace(b, a)
            .Replace('_', b)

    match instruction with
    | Spin v ->
        let a = programs.Substring(0, programs.Length - v)
        let b = programs.Substring(programs.Length - v)
        b + a
    | Exchange (a, b) ->
        partner programs.[a] programs.[b] programs
    | Partner (a, b) ->
        partner a b programs

let solution1 =
    input
    |> Seq.fold applyInstruction initial

let solution2 =
    let repetitions = 1_000_000_000
    let cycle =
        input
        |> Seq.replicate repetitions
        |> Seq.collect id
        |> Seq.scan applyInstruction initial
        |> Seq.skip 1
        |> Seq.takeWhile ((<>) initial)
        |> Seq.append [ initial ]
        |> Seq.toArray
    cycle.[(repetitions % cycle.Length)]
