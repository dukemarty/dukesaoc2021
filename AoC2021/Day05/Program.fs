// Learn more about F# at http://fsharp.org

open System

let expandHorizVertLines line =
    let max (a, b) = if (a > b) then a else b
    let min (a, b) = if (a < b) then a else b

    match line with
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromX = toX ->
        printfn "Found vertical line: %A" line

        seq { for y in min (fromY, toY) .. max (fromY, toY) -> y }
        |> Seq.map (fun y -> (fromX, y))
        |> List.ofSeq
    | (fromX :: fromY :: _) :: (toX :: toY :: _) :: _ when fromY = toY ->
        printfn "Found horizontal line: %A" line

        seq { for x in min (fromX, toX) .. max (fromX, toX) -> x }
        |> Seq.map (fun x -> (x, fromY))
        |> List.ofSeq
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
    printfn "Lines points: %A" horizVert

    let res = countOverlappedPoints horizVert
    printfn "Part 1, number of overlapping fields: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 5: Hydrothermal Venture\n===========================\n"
    printfn "All lines: %A" (DataInput.lines |> List.ofSeq)
    part1ManhattanLines DataInput.lines
    0 // return an integer exit code
