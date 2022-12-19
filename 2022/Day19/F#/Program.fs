open System
open System.IO
open System.Text.RegularExpressions

type Resource = Ore | Clay | Obsidian | Geode

let parseResource = function
    | "ore" -> Ore
    | "clay" -> Clay
    | "obsidian" -> Obsidian
    | "geode" -> Geode
    | v -> failwith $"Invalid resource: \"%s{v}\""

type Resources = {
    Ore: int
    Clay: int
    Obsidian: int
    Geode: int
}
module Resources =
    let zero = { Ore = 0; Clay = 0; Obsidian = 0; Geode = 0 }
    let addResource (resource, amount) resources =
        match resource with
        | Ore -> { resources with Ore = resources.Ore + amount }
        | Clay -> { resources with Clay = resources.Clay + amount }
        | Obsidian -> { resources with Obsidian = resources.Obsidian + amount }
        | Geode -> { resources with Geode = resources.Geode + amount }
    let add r1 r2 =
        {
            Ore = r1.Ore + r2.Ore
            Clay = r1.Clay + r2.Clay
            Obsidian = r1.Obsidian + r2.Obsidian
            Geode = r1.Geode + r2.Geode
        }
    let trySubtract r1 r2 =
        if r1.Ore >= r2.Ore &&
            r1.Clay >= r2.Clay &&
            r1.Obsidian >= r2.Obsidian &&
            r1.Geode >= r2.Geode then
            Some {
                Ore = r1.Ore - r2.Ore
                Clay = r1.Clay - r2.Clay
                Obsidian = r1.Obsidian - r2.Obsidian
                Geode = r1.Geode - r2.Geode
            }
        else None

type BlueprintEntry = {
    RobotType: Resource
    Costs: Resources
}

let parseBlueprintEntry (v: string) =
    let m = Regex.Match(v, @"^Each (?<robotType>\w+) robot costs (?<amount1>\d+) (?<type1>\w+)( and (?<amount2>\d+) (?<type2>\w+))?$")
    if not m.Success then failwith "Can't parse blueprint entry"
    {
        RobotType = parseResource m.Groups.["robotType"].Value
        Costs =
            Resources.zero
            |> Resources.addResource (parseResource m.Groups.["type1"].Value, int m.Groups.["amount1"].Value)
            |> fun r ->
                if m.Groups.["type2"].Success then
                    Resources.addResource (parseResource m.Groups.["type2"].Value, int m.Groups.["amount2"].Value) r
                else r
    }

type Blueprint = {
    Id: int
    Entries: BlueprintEntry list
}

let parseBlueprint (v: string) =
    let m = Regex.Match(v, @"^Blueprint (\d+): (.*)$")
    if not m.Success then failwith "Can't parse blueprint"
    {
        Id = int m.Groups.[1].Value
        Entries =
            m.Groups.[2].Value.Split('.', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Seq.map parseBlueprintEntry
            |> Seq.toList
    }

type State = {
    CollectingRobots: Resources
    NewRobots: Resources
    Resources: Resources
}

let collectResources state =
    { state with Resources = Resources.add state.CollectingRobots state.Resources }

let beginCollecting state =
    { state with
        NewRobots = Resources.zero
        CollectingRobots = Resources.add state.CollectingRobots state.NewRobots
    }

let tryBuy state blueprintEntry =
    match Resources.trySubtract state.Resources blueprintEntry.Costs with
    | Some remainingResources ->
        { state with
            NewRobots = Resources.addResource (blueprintEntry.RobotType, 1) state.NewRobots
            Resources = remainingResources
        }
        |> Some
    | None -> None

let canBuyMultipleRobots blueprint state =
    seq {
        let entries = blueprint.Entries
        for i in 0..entries.Length-1 do
        for j in i..entries.Length-1 do
            Resources.add entries.[i].Costs entries.[j].Costs
    }
    |> Seq.tryPick (Resources.trySubtract state.Resources)
    |> Option.isSome

let rec getNextStates blueprint state =
    seq {
        if not <| canBuyMultipleRobots blueprint state then yield state
        yield!
            blueprint.Entries
            |> Seq.choose (tryBuy state)
    }

let sum1ToN n =
    n * (n + 1) / 2

let getMaxGeodes minutesLeft blueprint =
    let rec fn minutesLeft states =
        if minutesLeft = 1 then
            states
            |> Seq.map (collectResources >> fun s -> s.Resources.Geode)
            |> Seq.max
        else
            let states' =
                states
                |> Seq.collect (getNextStates blueprint)
                |> Seq.map (collectResources >> beginCollecting)
                |> Seq.distinct
                |> Seq.toList
            let maxState =
                states'
                |> List.map (fun s -> s.Resources.Geode + (minutesLeft - 1) * s.CollectingRobots.Geode)
                |> List.max
            let states'' =
                states'
                |> List.filter (fun s -> s.Resources.Geode + (minutesLeft - 1) * s.CollectingRobots.Geode + sum1ToN (minutesLeft - 1) > maxState)
            fn (minutesLeft - 1) states''
    [
        { CollectingRobots = { Resources.zero with Ore = 1 }; NewRobots = Resources.zero; Resources = Resources.zero }
    ]
    |> fn minutesLeft

let getQualityLevel blueprint =
    blueprint.Id * (getMaxGeodes 24 blueprint)

let blueprints =
    File.ReadLines("input.txt")
    |> Seq.map parseBlueprint
    |> Seq.toList

blueprints
|> List.sumBy getQualityLevel
|> printfn "Part 1: %d"

blueprints
|> List.truncate 3
|> List.map (getMaxGeodes 32)
|> List.reduce ((*))
|> printfn "Part 2: %d"
