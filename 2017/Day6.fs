let input =
    "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4".Split(' ')
    |> Seq.map int
    |> Seq.toList

let folder configs =
    let newConfig =
        let result =
            configs
            |> List.head
            |> Array.ofList

        let (maxValueIdx, maxValue) =
            result
            |> Seq.mapi (fun index value -> index, value)
            |> Seq.maxBy (fun (index, value) -> value, -index)

        Array.set result maxValueIdx 0

        Seq.init maxValue (fun i -> (maxValueIdx + 1 + i) % result.Length)
        |> Seq.iter (fun i -> Array.get result i + 1 |> Array.set result i)

        result |> Array.toList

    newConfig :: configs

let configWithDuplicatedItems =
    Seq.initInfinite ignore
    |> Seq.scan (fun configs () -> folder configs) [input]
    |> Seq.mapi (fun idx l -> idx, l)
    |> Seq.find (fun (_, configs) -> List.tail configs |> List.contains (List.head configs))

let solution1 =
    fst configWithDuplicatedItems

let solution2 =
    let config = snd configWithDuplicatedItems

    let firstIndex =
        config
        |> List.tail
        |> List.findIndex ((=) (List.head config))

    firstIndex + 1
