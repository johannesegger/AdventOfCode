let input = 368078

let solution1 =
    let abs (x: int) = System.Math.Abs x
    let (circleIdx, startValue) =
        Seq.initInfinite id
        |> Seq.scan (fun (i, n) _ -> i + 1, n + i * 8 + 1) (1, 2)
        |> Seq.takeWhile (snd >> ((>=) input))
        |> Seq.last
    let rightTop = startValue + circleIdx
    let leftTop = rightTop + 2 * circleIdx
    let leftBottom = leftTop + 2 * circleIdx
    let rightBottom = leftBottom + 2 * circleIdx + 1
    let (x, y) =
        if rightBottom <= input then (circleIdx + 1), (circleIdx - (input - rightBottom))
        elif leftBottom <= input then abs(input - (leftBottom + circleIdx)), circleIdx
        elif leftTop <= input then circleIdx, abs(input - (leftTop + circleIdx))
        elif rightTop <= input then abs(input - (rightTop + circleIdx)), circleIdx
        else circleIdx, input - startValue
    x + y

let solution2 =
    let move values (x, y) =
        let value =
            [for dx in -1..1 do for dy in -1..1 -> x + dx, y + dy]
            |> List.choose (fun p -> Map.tryFind p values)
            |> List.sum
        Map.add (x, y) value values

    let iterate values i =
        let positions =
            [for y in -i+1..i -> i, y]
            @
            [for x in i-1..-1..-i -> x, i]
            @
            [for y in i-1..-1..-i -> -i, y]
            @
            [for x in -i+1..i -> x, -i]
        positions
        |> List.fold move values

    Seq.initInfinite ((+) 1)
    |> Seq.scan iterate (Map.empty |> Map.add (0, 0) 1)
    |> Seq.choose (Map.toSeq >> Seq.map snd >> Seq.filter ((<) input) >> Seq.sort >> Seq.tryHead)
    |> Seq.head
    