let input = "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118"

type Data = {
    List: byte list
    CurrentPosition: int
    SkipSize: int
}

let defaultData = {
    List = [0uy..255uy]
    CurrentPosition = 0
    SkipSize = 0
}

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

let solution1 =
    input.Split(',')
    |> Seq.map byte
    |> Seq.toList
    |> List.fold folder defaultData
    |> fun d -> (int d.List.[0]) * (int d.List.[1])

let solution2 =
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
