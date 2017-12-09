let doorId = "ffykfhsq"

let md5 = System.Security.Cryptography.MD5.Create()

let hashes =
    Seq.initInfinite (fun i ->
        sprintf "%s%d" doorId i
        |> System.Text.Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> System.BitConverter.ToString
        |> fun s -> s.Replace("-", "")
    )
    |> Seq.filter (fun hash -> hash.StartsWith "00000")
    |> Seq.cache

let solution1 =
    hashes
    |> Seq.map (fun hash -> hash.[5])
    |> Seq.take 8
    |> Seq.toArray
    |> System.String

let solution2 =
    let add map (position, character) =
        if position >= '0' && position <= '7'
        then
            match Map.tryFind position map with
            | None -> Map.add position character map
            | Some _ -> map
        else
            map
    hashes
    |> Seq.map (fun hash -> hash.[5], hash.[6])
    |> Seq.scan add Map.empty
    |> Seq.filter (Map.count >> ((=) 8))
    |> Seq.head
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.toArray
    |> System.String
    