// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1LowestTotalRisk (data: int array array) =
    let h, w = data.Length, data.[0].Length

    let dists =
        seq { for i in 1 .. h -> i }
        |> Seq.map (fun _ -> Array.create w -1)
        |> Array.ofSeq

    let updateNeighbours r c =
        let neighbours =
            MatrixOperations.createValidNeighboursSimple (r, c) w h

        let baseVal = dists.[r].[c]
        let mutable res = []

        for (nr, nc) in neighbours do
            if ((dists.[nr].[nc] < 0)
                || ((baseVal + data.[nr].[nc]) < dists.[nr].[nc])) then
                dists.[nr].[nc] <- baseVal + data.[nr].[nc]
                res <- (nr, nc) :: res

        res


    let rec pathFindHelper openPositions =
        match openPositions with
        | (xr, xc) :: xs ->
            let addPos = updateNeighbours xr xc
            pathFindHelper (xs @ addPos)
        | [] -> dists

    dists.[0].[0] <- 0
    let initialOpenPositions = updateNeighbours 0 0
    ignore (pathFindHelper initialOpenPositions)
    printfn "Part 1, lowest total risk: %d" dists.[h - 1].[w - 1]

let part2LargeLowestTotalRisk (data: int array array) =
    let h, w = data.Length, data.[0].Length
    let fh, fw = 5 * h, 5 * w
    
    let calcBase (row, col) =
        let incr = (row / h) + (col / w)
        let realR, realC = (row % h), (col % w)
        let increasedVal = data.[realR].[realC] + incr
        ((increasedVal - 1) % 9) + 1

    let realData =
        seq { for i in 1 .. fh -> i }
        |> Seq.map (fun _ -> Array.create fw -1)
        |> Array.ofSeq

    for rr in 0 .. (fh - 1) do
        for rc in 0 .. (fw - 1) do
            realData.[rr].[rc] <- calcBase (rr, rc)

    let dists =
        seq { for i in 1 .. fh -> i }
        |> Seq.map (fun _ -> Array.create fw -1)
        |> Array.ofSeq

    let updateNeighbours r c =
        let neighbours =
            MatrixOperations.createValidNeighboursSimple (r, c) fw fh

        let baseVal = dists.[r].[c]
        let mutable res = []

        for (nr, nc) in neighbours do
            if ((dists.[nr].[nc] < 0)
                || ((baseVal + realData.[nr].[nc]) < dists.[nr].[nc])) then
                dists.[nr].[nc] <- baseVal + realData.[nr].[nc]
                res <- (nr, nc) :: res

        res


    let rec pathFindHelper openPositions =
        match openPositions with
        | (xr, xc) :: xs ->
            let addPos = updateNeighbours xr xc
            pathFindHelper (xs @ addPos)
        | [] -> dists

    dists.[0].[0] <- 0
    let initialOpenPositions = updateNeighbours 0 0
    ignore (pathFindHelper initialOpenPositions)
    printfn "Part 2, lowest total risk: %d" dists.[fh - 1].[fw - 1]

[<EntryPoint>]
let main argv =
    printfn "Day 15: Chiton\n==============\n"
    let data = DataInput.intMatrix DataInput.Puzzle
    //printfn "Data: %A" data
    part1LowestTotalRisk data
    part2LargeLowestTotalRisk data
    0 // return an integer exit code
