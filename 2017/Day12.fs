let parseLine line =
    let m = System.Text.RegularExpressions.Regex.Match(line, @"^(?<program>\d+) <-> (?<programs>.*)$")
    if not m.Success then failwithf "Line can't be parsed: '%s'" line
    int m.Groups.["program"].Value
    , m.Groups.["programs"].Value.Split(',') |> Seq.map int |> Seq.toList

let folder map (program, programs) =
    let addChannel map p2 =
        let list =
            match Map.tryFind program map with
            | None -> Set.empty
            | Some l -> l
        Map.add program (Set.add p2 list) map

    programs
    |> Seq.fold addChannel map

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day12-input.txt")
    |> Seq.map parseLine
    |> Seq.fold folder Map.empty

let rec findChannels map visited c =
    if visited |> Set.contains c
    then visited
    else
        let visited' = Set.add c visited
        Map.find c map
        |> Set.map (findChannels map visited')
        |> Set.unionMany

let solution1 =
    findChannels input Set.empty 0
    |> Set.count

let solution2 =
    let programs =
        input
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq

    let folder' remainingPrograms =
        let group = findChannels input Set.empty (Set.minElement remainingPrograms)
        Set.difference remainingPrograms group

    Seq.initInfinite ignore
    |> Seq.scan (fun s () -> folder' s) programs
    |> Seq.takeWhile (Set.isEmpty >> not)
    |> Seq.length
