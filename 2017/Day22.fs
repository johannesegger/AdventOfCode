type Position = int * int

type Direction = Up | Down | Left | Right

type Carrier = Position * Direction

type InfectionState = Weakened | Infected | Flagged

let turnLeft = function
    | Up -> Left
    | Down -> Right
    | Left -> Down
    | Right -> Up

let turnRight = function
    | Up -> Right
    | Down -> Left
    | Left -> Up
    | Right -> Down

let turnAround = function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let parseLine y line =
    line
    |> Seq.indexed
    |> Seq.filter (snd >> ((=)'#'))
    |> Seq.map (fst >> fun x -> x, y)
    |> Set.ofSeq

let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\Day22-input.txt")
    
let initialInfectedNodes =
    input
    |> Seq.mapi parseLine
    |> Set.unionMany

let initialCarrier : Carrier =
    let center = Array.length input / 2
    (center, center), Up

let move direction (x, y) =
    match direction with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

module Solution1 =
    type Data =  {
        BurstsWithInfection: int
        Carrier: Carrier
        InfectedNodes: Position Set
    }

    let iterate data =
        let (carrierPosition, carrierDirection) = data.Carrier
        let (infectedNodes, direction, burstsWithInfection') =
            if Set.contains carrierPosition data.InfectedNodes
            then
                Set.remove carrierPosition data.InfectedNodes,
                turnRight carrierDirection,
                data.BurstsWithInfection
            else
                Set.add carrierPosition data.InfectedNodes,
                turnLeft carrierDirection,
                data.BurstsWithInfection + 1

        let carrier =
            move direction carrierPosition,
            direction
        {
            BurstsWithInfection = burstsWithInfection'
            Carrier = carrier
            InfectedNodes = infectedNodes
        }

    let result =
        let initialData = {
            BurstsWithInfection = 0
            Carrier = initialCarrier
            InfectedNodes = initialInfectedNodes
        }
        Seq.init 10000 ignore
        |> Seq.fold (fun p () -> iterate p) initialData
        |> fun p -> p.BurstsWithInfection

module Solution2 =
    type Data =  {
        BurstsWithInfection: int
        Carrier: Carrier
        InfectedNodes: Map<Position, InfectionState>
    }

    let iterate data =
        let (carrierPosition, carrierDirection) = data.Carrier
        let (infectedNodes, direction, burstsWithInfection') =
            match Map.tryFind carrierPosition data.InfectedNodes with
            | None ->
                Map.add carrierPosition Weakened data.InfectedNodes,
                turnLeft carrierDirection,
                data.BurstsWithInfection
            | Some Weakened ->
                Map.add carrierPosition Infected data.InfectedNodes,
                carrierDirection,
                data.BurstsWithInfection + 1
            | Some Infected ->
                Map.add carrierPosition Flagged data.InfectedNodes,
                turnRight carrierDirection,
                data.BurstsWithInfection
            | Some Flagged ->
                Map.remove carrierPosition data.InfectedNodes,
                turnAround carrierDirection,
                data.BurstsWithInfection
            
        let carrier =
            move direction carrierPosition,
            direction
        {
            BurstsWithInfection = burstsWithInfection'
            Carrier = carrier
            InfectedNodes = infectedNodes
        }

    let result =
        let initialData = {
            BurstsWithInfection = 0
            Carrier = initialCarrier
            InfectedNodes =
                initialInfectedNodes
                |> Seq.map (fun p -> p, Infected)
                |> Map.ofSeq
        }
        Seq.init 10_000_000 ignore
        |> Seq.fold (fun p () -> iterate p) initialData
        |> fun p -> p.BurstsWithInfection
