let parseLine (line: string) =
    line.Split(':')
    |> Array.map int
    |> fun a -> a.[0], a.[1]

let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day13-input.txt")


type MoveDirection = Up | Down 

type Scanner = {
    Position: int
    MoveDirection: MoveDirection
    Range: int
}

let scanners =
    input
    |> Seq.map parseLine
    |> Map.ofSeq
    |> Map.map (fun _ range -> { Position = 0; MoveDirection = Down; Range = range })

let moveScanner scanner =
    match scanner.MoveDirection with
    | Down when scanner.Position < scanner.Range - 1 ->
        { scanner with Position = scanner.Position + 1 }
    | Down ->
        { scanner with Position = scanner.Position - 1; MoveDirection = Up }
    | Up when scanner.Position > 0 ->
        { scanner with Position = scanner.Position - 1 }
    | Up ->
        { scanner with Position = scanner.Position + 1; MoveDirection = Down }

let moveScanners scanners =
    scanners
    |> Map.map (fun _ scanner -> moveScanner scanner)

let folder (scanners, totalCost) layer =
    let totalCost' =
        match Map.tryFind layer scanners with
        | Some scanner ->
            if scanner.Position = 0
            then totalCost + layer * scanner.Range
            else totalCost
        | None -> totalCost
    let scanners' = moveScanners scanners
    scanners', totalCost'

let numberOfLayers = (scanners |> Map.toSeq |> Seq.map fst |> Seq.max) + 1

let getCost scanners =
    Seq.init numberOfLayers id
    |> Seq.fold folder (scanners, 0)
    |> snd

let solution1 =
    getCost scanners

let isCaught scanners =
    let scanner0Catches =
        match Map.tryFind 0 scanners with
        | Some scanner -> scanner.Position = 0
        | None -> false
    scanner0Catches || getCost scanners > 0

let solution2 =
    let folder' scanners =
        let scanners' = moveScanners scanners
        scanners', isCaught scanners'
    Seq.initInfinite ignore
    |> Seq.scan (fun (scanners, _) () -> folder' scanners) (scanners, true)
    |> Seq.indexed
    |> Seq.find (snd >> snd >> not)
    |> fst
