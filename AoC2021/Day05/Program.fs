// Learn more about F# at http://fsharp.org

open AoC.Utils

let expandHorizVertLines line =
    match line with
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromX = toX ->
        //printfn "Found vertical line: %A" line

        seq { for y in Comparisons.min (fromY, toY) .. Comparisons.max (fromY, toY) -> y }
        |> Seq.map (fun y -> (fromX, y))
        |> List.ofSeq
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromY = toY ->
        //printfn "Found horizontal line: %A" line

        seq { for x in Comparisons.min (fromX, toX) .. Comparisons.max (fromX, toX) -> x }
        |> Seq.map (fun x -> (x, fromY))
        |> List.ofSeq
    | _ -> Seq.empty |> List.ofSeq


let expandDiagonalLine line =
    let calcDelta (v1, v2) = if (v1 > v2) then -1 else 1

    match line with
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromX = toX -> Seq.empty |> List.ofSeq
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromY = toY -> Seq.empty |> List.ofSeq
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ ->
        let (deltaX, deltaY) =
            (calcDelta (fromX, toX), calcDelta (fromY, toY))

        let mutable (currX, currY) = (fromX, fromY)
        let endPoint = (toX, toY)
        let mutable res = [ (currX, currY) ]

        while (not ((currX, currY) = endPoint)) do
            currX <- currX + deltaX
            currY <- currY + deltaY
            res <- (currX, currY) :: res

        res
    | _ -> Seq.empty |> List.ofSeq


let countOverlappedPoints points =
    let rec filterForMultiples rawList res run =
        match res with
        | l :: ls ->
            match rawList with
            | x1 :: x2 :: xs when run && x1 = l -> filterForMultiples (x2 :: xs) res true
            | x1 :: x2 :: xs when run && x1 = x2 -> filterForMultiples xs (x1 :: res) true
            | x1 :: x2 :: xs when not run && x1 = x2 -> filterForMultiples xs (x1 :: res) true
            | x :: xs -> filterForMultiples xs res false
            | [] -> res
        | [] ->
            match rawList with
            | x1 :: x2 :: xs when x1 = x2 -> filterForMultiples xs [ x1 ] true
            | x1 :: x2 :: xs when not (x1 = x2) -> filterForMultiples (x2 :: xs) [] false
            | _ -> res

    let pointsSorted = points |> List.sort
    //printfn "Sorted points: %A" pointsSorted
    let overlappedPoints = filterForMultiples pointsSorted [] false
    //printfn "Overlapped points: %A" overlappedPoints
    overlappedPoints.Length

let part1ManhattanLines lines =
    let horizVert =
        List.concat (lines |> Seq.map expandHorizVertLines)

    //printfn "Lines points: %A" horizVert

    let res = countOverlappedPoints horizVert
    printfn "Part 1, number of overlapping fields: %d" res

let part2AllLines lines =
    let horizVert =
        List.concat (lines |> Seq.map expandHorizVertLines)

    let diags =
        List.concat (lines |> Seq.map expandDiagonalLine)

    let allPoints = List.concat ([ horizVert; diags ])
    //printfn "Lines points: %A" allPoints

    let res = countOverlappedPoints allPoints
    printfn "Part 2, number of overlapping fields: %d" res


[<EntryPoint>]
let main argv =
    printfn "Day 5: Hydrothermal Venture\n===========================\n"
    //printfn "All lines: %A" (DataInput.lines |> List.ofSeq)
    part1ManhattanLines DataInput.lines
    part2AllLines DataInput.lines
    0 // return an integer exit code
