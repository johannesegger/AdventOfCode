let input = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\\Day9-input.txt")

type Data = {
    GroupScore: int
    TotalScore: int
    IsInsideGarbage: bool
    IsCanceled: bool
    GarbageCharacterCount: int
}

let defaultData = {
    GroupScore = 0
    TotalScore = 0
    IsInsideGarbage = false
    IsCanceled = false
    GarbageCharacterCount = 0
}

let folder data c =
    if data.IsCanceled then { data with IsCanceled = false }
    elif data.IsInsideGarbage && c = '>' then { data with IsInsideGarbage = false }
    elif c = '!' then { data with IsCanceled = true }
    elif data.IsInsideGarbage then { data with GarbageCharacterCount = data.GarbageCharacterCount + 1 }
    elif c = '<' then { data with IsInsideGarbage = true }
    elif c = '{' then { data with GroupScore = data.GroupScore + 1 }
    elif c = '}' then { data with GroupScore = data.GroupScore - 1; TotalScore = data.TotalScore + data.GroupScore }
    else data

let foldedData =
    input
    |> Seq.fold folder defaultData

let solution1 =
    foldedData.TotalScore

let solution2 =
    foldedData.GarbageCharacterCount
