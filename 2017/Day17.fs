let input = 371

type CircularBuffer = {
    Data: int list
    Position: int
}

let solution1 =
    let folder buffer number =
        let insertPosition = (buffer.Position + input) % buffer.Data.Length + 1
        let (a, b) = List.splitAt insertPosition buffer.Data
        { buffer with Data = a @ (number :: b); Position = insertPosition }

    Seq.init 2017 ((+)1)
    |> Seq.fold folder { Data = [0]; Position = 0 }
    |> fun p -> List.item (p.Position + 1) p.Data

type VirtualCircularBuffer = {
    Position: int
    ElementAtPos1: int
}

let solution2 =
    let folder buffer number =
        let length = number
        let insertPosition = (buffer.Position + input) % length + 1
        {
            Position = insertPosition
            ElementAtPos1 = if insertPosition = 1 then number else buffer.ElementAtPos1
        }

    Seq.init 50_000_000 ((+)1)
    |> Seq.fold folder { Position = 0; ElementAtPos1 = -1 }
    |> fun p -> p.ElementAtPos1
