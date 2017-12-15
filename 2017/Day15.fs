type Generator = {
    StartValue: int
    Factor: int
    Filter: int -> bool
}

let generatorA = { StartValue = 873; Factor = 16807; Filter = fun _ -> true }
let generatorB = { StartValue = 583; Factor = 48271; Filter = fun _ -> true }

let generate generator =
    let folder (value: int) =
        ((int64 value) * (int64 generator.Factor)) % 2147483647L 
    Seq.initInfinite ignore
    |> Seq.scan (fun v () -> folder v |> int) generator.StartValue
    |> Seq.filter generator.Filter
    |> Seq.skip 1

let both fn (a, b) = fn a, fn b

let apply fn (a, b) = fn a b

let getBytes (v: int) =
    let l =
        System.BitConverter.GetBytes v
        |> Array.toList
    if System.BitConverter.IsLittleEndian
    then l
    else List.rev l

let rec getLowest2Bytes = function
    | h :: t :: _ -> [t; h]
    | l -> l

let solve generatorA generatorB pairs =
    Seq.zip (generate generatorA) (generate generatorB)
    |> Seq.map (both (getBytes >> getLowest2Bytes))
    |> Seq.take pairs
    |> Seq.filter (apply (=))
    |> Seq.length

let solution1 =
    solve generatorA generatorB 40_000_000

let solution2 =
    let generatorA = { generatorA with Filter = fun v -> v % 4 = 0 }
    let generatorB = { generatorB with Filter = fun v -> v % 8 = 0 }
    solve generatorA generatorB 5_000_000
