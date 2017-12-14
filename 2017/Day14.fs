let input = "wenycdww"

type KnotHashData = {
    List: byte list
    CurrentPosition: int
    SkipSize: int
}

let knotHash dataBytes (input: string) =
    let folder data (length: byte) =
        let intLength = int length
        let list =
            if data.CurrentPosition + intLength <= data.List.Length
            then
                let span =
                    data.List.[data.CurrentPosition..data.CurrentPosition + intLength - 1]
                    |> List.rev
                data.List.[0..data.CurrentPosition - 1]
                @
                span
                @
                data.List.[data.CurrentPosition + intLength..]
            else
                let startLength = intLength - (data.List.Length - data.CurrentPosition)
                let endLength = intLength - startLength
                let span =
                    (
                        data.List.[data.CurrentPosition..]
                        @
                        data.List.[0..startLength - 1]
                    )
                    |> List.rev
                
                span.[endLength..]
                @
                data.List.[startLength..data.List.Length - intLength + startLength - 1]
                @
                span.[0..endLength - 1]
        { data with
            List = list
            CurrentPosition = (data.CurrentPosition + intLength + data.SkipSize) % data.List.Length
            SkipSize = data.SkipSize + 1
        }

    let defaultData = {
        List = dataBytes
        CurrentPosition = 0
        SkipSize = 0
    }

    let lengths =
        (
            input
            |> System.Text.Encoding.ASCII.GetBytes
            |> Array.toList
        )
        @
        [17uy; 31uy; 73uy; 47uy; 23uy]
    lengths
    |> List.replicate 64
    |> List.collect id
    |> List.fold folder defaultData
    |> fun d -> d.List
    |> List.chunkBySize 16
    |> List.map (List.reduce (^^^) >> (sprintf "%02x"))
    |> String.concat ""

let getBits (s: string) =
    let byteFromHexChar (c: char) =
        System.Convert.ToByte(string c, 16)
    let byteToBits (b: byte) =
        System.Convert.ToString(b, 2).PadLeft(4, '0')
    s
    |> Seq.map (byteFromHexChar >> byteToBits)
    |> String.concat ""

let grid =
    Seq.init 128 (sprintf "%s-%d" input)
    |> Seq.map (knotHash [0uy..255uy] >> getBits)
    |> Seq.toList

let solution1 =
    grid
    |> Seq.sumBy (fun l -> l |> Seq.sumBy (fun c -> if c = '1' then 1 else 0))

let solution2 =
    let used =
        grid
        |> Seq.mapi (fun rowIdx line ->
            line
            |> Seq.mapi (fun colIdx c ->
                if c = '1' then Some (rowIdx, colIdx)
                else None
            )
            |> Seq.choose id
        )
        |> Seq.collect id
        |> Seq.toList


    let rec updateGroup (rowIdx, colIdx) group map =
        let map' =
            Map.add (rowIdx, colIdx) group map
        
        let folder map cell =
            match Map.tryFind cell map with
            | Some group' when group' <> group -> updateGroup cell group map
            | _ -> map
        
        [
            (rowIdx - 1, colIdx)
            (rowIdx, colIdx + 1)
            (rowIdx + 1, colIdx)
            (rowIdx, colIdx - 1)
        ]
        |> List.fold folder map'

    let folder map (rowIdx, colIdx) =
        let group =
            Map.tryFind (rowIdx, colIdx - 1) map
            |> Option.orElse (Map.tryFind (rowIdx - 1, colIdx) map)

        match group with
        | Some group -> updateGroup (rowIdx, colIdx) group map
        | None -> Map.add (rowIdx, colIdx) (System.Guid.NewGuid()) map

    used
    |> Seq.fold folder Map.empty
    |> Map.toSeq
    |> Seq.distinctBy snd
    |> Seq.length
