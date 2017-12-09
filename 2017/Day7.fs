let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day7-input.txt")
    |> Seq.map (fun line ->
        let m = System.Text.RegularExpressions.Regex.Match(line, @"^(?<name>\w+)\s*\((?<weight>\d+)\)(\s*->\s*(?<children>.*))?$")
        
        let children =
            if m.Groups.["children"].Value.Trim() = ""
            then Set.empty
            else m.Groups.["children"].Value.Split(',') |> Seq.map (fun p -> p.Trim()) |> Set.ofSeq
        
        m.Groups.["name"].Value
        , int m.Groups.["weight"].Value
        , children
    )
    |> Seq.toList

let solution1 =
    let names =
        input
        |> Seq.map (fun (name, _, _) -> name)
        |> Set.ofSeq
    let children =
        input
        |> Seq.collect (fun (_, _, children) -> children)
        |> Set.ofSeq

    Set.difference names children
    |> Set.toList
    |> List.exactlyOne
    
let solution2 =
    let rec getWeights names =
        let getWeight parent =
            input
            |> List.find (fun (name, _, _) -> name = parent)
            |> fun (name, weight, children) ->
                let childWeights = getWeights children
                
                if childWeights |> List.map snd |> Set.ofList |> Set.count > 1
                then
                    // TODO find the unbalanced child and look what its own weight would need to be
                    // I had to do it manually :-(
                    failwithf "%s, Child weights: %A" name childWeights
                
                name
                , (List.sumBy snd childWeights) + weight
        names |> Seq.map getWeight |> Seq.toList
    getWeights (Set.singleton solution1)
