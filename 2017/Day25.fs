type State = A | B | C | D | E | F

type Value = Zero | One

type TransitionAction =
    | Write of Value
    | Move of int
    | SetState of State

let steps = 12386363

let transition state value =
    match state, value with
    | A, Zero -> [ Write One; Move 1; SetState B ]
    | A, One -> [ Write Zero; Move -1; SetState E ]
    | B, Zero -> [ Write One; Move -1; SetState C ]
    | B, One -> [ Write Zero; Move 1; SetState A ]
    | C, Zero -> [ Write One; Move -1; SetState D ]
    | C, One -> [ Write Zero; Move 1; SetState C ]
    | D, Zero -> [ Write One; Move -1; SetState E ]
    | D, One -> [ Write Zero; Move -1; SetState F ]
    | E, Zero -> [ Write One; Move -1; SetState A ]
    | E, One -> [ Write One; Move -1; SetState C ]
    | F, Zero -> [ Write One; Move -1; SetState E ]
    | F, One -> [ Write One; Move 1; SetState A ]

type Data = {
    State: State
    Position: int
    Registers: Set<int>
}

let executeAction data = function
    | Write Zero ->
        { data with
            Registers = Set.remove data.Position data.Registers
        }
    | Write One ->
        { data with
            Registers = Set.add data.Position data.Registers
        }
    | Move offset ->
        { data with
            Position = data.Position + offset
        }
    | SetState state ->
        { data with
            State = state
        }

let iterate data =
    let value =
        if Set.contains data.Position data.Registers then One
        else Zero

    transition data.State value
    |> Seq.fold executeAction data

let solution1 =
    let initialData = {
        State = A
        Position = 0
        Registers = Set.empty
    }

    Seq.init steps ignore
    |> Seq.fold (fun p () -> iterate p) initialData
    |> fun p -> p.Registers |> Set.count
