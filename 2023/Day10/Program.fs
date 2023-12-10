open System.IO

type PipeOpening = North | South | West | East

let parsePipe = function
    | '|' -> Some (North, South)
    | '-' -> Some (East, West)
    | 'L' -> Some (North, East)
    | 'J' -> Some (North, West)
    | '7' -> Some (South, West)
    | 'F' -> Some (South, East)
    | '.' -> None
    | v -> failwith $"Invalid character: %c{v}"

let hasOpening opening pipe =
    match pipe with
    | Some (a, b) -> a = opening || b = opening
    | None -> false

let getStartPipe north south east west =
    [
        if north |> hasOpening South then North
        if south |> hasOpening North then South
        if west |> hasOpening East then West
        if east |> hasOpening West then East
    ]
    |> function
    | [a; b] -> (a, b)
    | openings -> failwith $"Can't parse start pipe with openings: %A{openings}"

let parseMap (lines: string array) =
    let map = Array2D.init lines.Length lines.[0].Length (fun row col ->
        if lines.[row].[col] = 'S' then
            getStartPipe
                (parsePipe lines.[row - 1].[col])
                (parsePipe lines.[row + 1].[col])
                (parsePipe lines.[row].[col - 1])
                (parsePipe lines.[row].[col + 1])
            |> Some
        else
            parsePipe lines.[row].[col]
    )
    let start =
        lines
        |> Seq.indexed
        |> Seq.pick (fun (rowIndex, row) ->
            row
            |> Seq.tryFindIndex ((=) 'S')
            |> Option.map (fun col -> rowIndex, col)
        )
    (map, start)

let getMaxDistance minDistances =
    minDistances
    |> Seq.cast<int option>
    |> Seq.choose id
    |> Seq.max

let getItem (row, col) items =
    Array2D.get items row col

let setItem (row, col) value items =
    Array2D.set items row col value
    items

let isValidPosition map (row, col) =
    row >= 0 && row < (Array2D.length1 map) && col >= 0 && col < (Array2D.length2 map)

let getConnectedNeighbors (map: _ array2d) (row, col) =
    [
        (North, South, (row - 1, col))
        (South, North, (row + 1, col))
        (West, East, (row, col - 1))
        (East, West, (row, col + 1))
    ]
    |> List.filter (fun (thisOpening, neighborOpening, position) ->
        isValidPosition map position &&
        getItem (row, col) map |> hasOpening thisOpening &&
        getItem position map |> hasOpening neighborOpening
    )
    |> List.map (fun (_, _, position) -> position)

let getConnectedNeighborsWithMinDistance (map, minDistances) position =
    let distance = (getItem position minDistances |> Option.get) + 1
    getConnectedNeighbors map position
    |> List.filter (fun next ->
        match getItem next minDistances with
        | Some minDistance when minDistance < distance -> false
        | Some _
        | None -> true
    )
    |> List.map (fun position -> (position, distance))

let findMinDistances (map, startingPoint) =
    let rec fn positions minDistances =
        if List.isEmpty positions then minDistances
        else
            let positions' =
                positions
                |> List.collect (getConnectedNeighborsWithMinDistance (map, minDistances))
            let minDistances' =
                positions'
                |> List.fold (fun a (p, d) -> setItem p (Some d) a) minDistances
            fn (List.map fst positions') minDistances'

    let minDistances =
        Array2D.create (Array2D.length1 map) (Array2D.length2 map) None
        |> setItem startingPoint (Some 0)
    fn [ startingPoint ] minDistances

File.ReadAllLines "input.txt"
|> parseMap
|> findMinDistances
|> getMaxDistance
|> printfn "Part 1: %d"

type TileType = Pipe of int | Outside

type MarkMode =
    | NormalMarkMode of (int * int)
    | SqueezeMarkMode of (int * int) * (int * int)

let getUnmarkedNeighborPositions (tileTypes: _ array2d) (row, col) =
    [
        (row - 1, col - 1)
        (row - 1, col)
        (row - 1, col + 1)
        (row, col - 1)
        (row, col + 1)
        (row + 1, col - 1)
        (row + 1, col)
        (row + 1, col + 1)
    ]
    |> List.filter (isValidPosition tileTypes)
    |> List.filter (fun position -> getItem position tileTypes = None)

let getPossibleSqueezePositions (tileTypes: _ array2d) (row, col) =
    [
        (row - 1, col - 1), (row - 1, col)
        (row - 1, col), (row - 1, col + 1)
        (row - 1, col + 1), (row, col + 1)
        (row, col + 1), (row + 1, col + 1)
        (row + 1, col + 1), (row + 1, col)
        (row + 1, col), (row + 1, col - 1)
        (row + 1, col - 1), (row, col - 1)
        (row, col - 1), (row - 1, col - 1)

        (row - 1, col), (row, col)
        (row, col + 1), (row, col)
        (row + 1, col), (row, col)
        (row, col - 1), (row, col)
    ]
    |> List.map (fun (a, b) -> if a < b then (a, b) else (b, a)) // necessary for intersecting positions
    |> List.filter (fun (a, b) -> isValidPosition tileTypes a && isValidPosition tileTypes b)
    |> List.filter (fun (a, b) ->
        match getItem a tileTypes, getItem b tileTypes with
        | Some (Pipe distanceToA), Some (Pipe distanceToB) ->
            abs (distanceToA - distanceToB) > 1
        | _, _ -> false
    )

let getNextPositions tileTypes markMode =
    match markMode with
    | NormalMarkMode position ->
        [
            yield! getUnmarkedNeighborPositions tileTypes position |> List.map NormalMarkMode
            yield! getPossibleSqueezePositions tileTypes position |> List.map SqueezeMarkMode
        ]
    | SqueezeMarkMode (a, b) ->
        [
            yield!
                Set.intersect
                    (getUnmarkedNeighborPositions tileTypes a |> Set.ofList)
                    (getUnmarkedNeighborPositions tileTypes b |> Set.ofList)
                |> Seq.map NormalMarkMode

            yield!
                Set.intersect
                    (getPossibleSqueezePositions tileTypes a |> Set.ofList)
                    (getPossibleSqueezePositions tileTypes b |> Set.ofList)
                |> Seq.map SqueezeMarkMode
        ]

let markOutsideTiles tileTypes startPosition =
    let rec fn positions visitedPositions tileTypes =
        if Set.isEmpty positions then tileTypes
        else
            let positions' =
                positions
                |> Seq.collect (getNextPositions tileTypes)
                |> Set.ofSeq
                |> fun p -> Set.difference p visitedPositions
            let visitedPositions = Set.union positions' visitedPositions
            let tileTypes' =
                positions'
                |> Seq.choose (function
                    | NormalMarkMode position -> Some position
                    | SqueezeMarkMode _ -> None
                )
                |> Seq.fold (fun map p -> setItem p (Some Outside) map) tileTypes
            fn positions' visitedPositions tileTypes'

    let positions = Set.ofList [ NormalMarkMode startPosition ]
    fn positions positions (setItem startPosition (Some Outside) tileTypes)

let findEnclosedTiles minDistances =
    let tileTypes =
        Array2D.init (Array2D.length1 minDistances) (Array2D.length2 minDistances) (fun row col ->
            match getItem (row, col) minDistances with
            | Some d -> Some (Pipe d)
            | _ -> None
        )
    let startPositions =
        [
            let maxRow = Array2D.length1 minDistances - 1
            let maxCol = Array2D.length2 minDistances - 1
            for col in 0 .. maxCol do
                (0, col)
            for row in 1 .. maxRow - 1 do
                (row, 0)
                (row, maxCol)
            for col in 0 .. maxCol do
                (maxRow, col)
        ]
        |> List.filter (fun position -> getItem position minDistances = None)
    startPositions
    |> List.fold markOutsideTiles tileTypes
    |> Seq.cast<TileType option>
    |> Seq.filter Option.isNone
    |> Seq.length

File.ReadAllLines "input.txt"
|> parseMap
|> findMinDistances
|> findEnclosedTiles
|> printfn "Part 2: %d"
