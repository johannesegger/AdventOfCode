let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day4-input.txt")

let pattern = System.Text.RegularExpressions.Regex @"^(?<name>(([a-z])+-)+)(?<sectorId>\d+)\[(?<checksum>[a-z]+)\]$"

let validRooms =
    input
    |> Seq.choose (fun line ->
        let m = pattern.Match line
        if m.Success
        then
            Some
                (
                    m.Groups.["name"].Value.TrimEnd '-',
                    m.Groups.["sectorId"].Value |> int,
                    m.Groups.["checksum"].Value
                )
        else
            None
    )
    |> Seq.choose (fun (name, sectorId, checksum) ->
        let actualChecksum =
            name.Replace("-", "")
            |> Seq.groupBy id
            |> Seq.sortBy (fun (key, items) -> Seq.length items * -1, key)
            |> Seq.map fst
            |> Seq.take checksum.Length
            |> Seq.toArray
            |> System.String
        if checksum = actualChecksum
        then Some (name, sectorId)
        else None
    )

let solution1 =
    validRooms
    |> Seq.sumBy snd

let solution2 =
    validRooms
    |> Seq.choose (fun (name, sectorId) ->
        let decrypted =
            name.ToLower()
            |> Seq.map (fun c ->
                if c = '-'
                then ' '
                else ((int c) - (int 'a') + sectorId) % 26 + (int 'a') |> char
            )
            |> Seq.toArray
            |> System.String
        if decrypted.Contains "north"
        then Some (name, decrypted, sectorId)
        else None
    )
    |> Seq.toList
