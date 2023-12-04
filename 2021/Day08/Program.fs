open System.IO

let parse (line: string) =
    let entry = line.Split(" | ")
    let signal = entry.[0].Split() |> Seq.map Set.ofSeq |> Seq.toList
    let output = entry.[1].Split() |> Seq.map Set.ofSeq |> Seq.toList
    (signal, output)

let count1478 (digits: Set<char> list) =
    digits
    |> Seq.filter (fun digit -> digit.Count = 2 || digit.Count = 4 || digit.Count = 3 || digit.Count = 7)
    |> Seq.length

File.ReadAllLines "input.txt"
|> Seq.map parse
|> Seq.sumBy (snd >> count1478)
|> printfn "Part 1: %d"

let getDigitWires signalDigits =
    let oneWires = signalDigits |> Seq.find (fun v -> Set.count v = 2)
    let fourWires = signalDigits |> Seq.find (fun v -> Set.count v = 4)
    let sevenWires = signalDigits |> Seq.find (fun v -> Set.count v = 3)
    let eightWires = signalDigits |> Seq.find (fun v -> Set.count v = 7)
    let topWire = Set.difference sevenWires oneWires |> Seq.exactlyOne
    let twoThreeFiveWires = signalDigits |> Seq.filter (fun v -> Set.count v = 5)
    let middleWire = Set.intersectMany twoThreeFiveWires |> Set.intersect fourWires |> Seq.exactlyOne
    let bottomWire = Set.intersectMany twoThreeFiveWires |> Set.remove middleWire |> Set.remove topWire |> Seq.exactlyOne
    let zeroSixNineWires = signalDigits |> Seq.filter (fun v -> Set.count v = 6)
    let leftTopWire = Set.difference fourWires oneWires |> Set.remove middleWire |> Seq.exactlyOne
    let leftBottomWire = Set.difference eightWires (Set.union oneWires (Set.ofList [ topWire; middleWire; bottomWire; leftTopWire ])) |> Seq.exactlyOne
    let rightTopWire = Set.difference oneWires (Set.intersectMany zeroSixNineWires) |> Seq.exactlyOne
    let rightBottomWire = oneWires |> Set.remove rightTopWire |> Seq.exactlyOne
    Map.ofList [
        eightWires |> Set.remove middleWire, 0
        oneWires, 1
        eightWires |> Set.remove leftTopWire |> Set.remove rightBottomWire, 2
        eightWires |> Set.remove leftTopWire |> Set.remove leftBottomWire, 3
        fourWires, 4
        eightWires |> Set.remove rightTopWire |> Set.remove leftBottomWire, 5
        eightWires |> Set.remove rightTopWire, 6
        sevenWires, 7
        eightWires, 8
        eightWires |> Set.remove leftBottomWire, 9
    ]

let getOutputValue (signalDigits, outputDigits) =
    let digitWires = getDigitWires signalDigits
    (0, outputDigits)
    ||> List.fold (fun number wires ->
        let digit = Map.find wires digitWires
        number * 10 + digit
    )

File.ReadAllLines "input.txt"
|> Seq.map parse
|> Seq.sumBy getOutputValue
|> printfn "Part 2: %d"
