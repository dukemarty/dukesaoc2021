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

[<EntryPoint>]
let main argv =
    printfn "Day 15: Chiton\n==============\n"
    let data = DataInput.intMatrix DataInput.Puzzle
    //printfn "Data: %A" data
    let res = part1LowestTotalRisk data
    0 // return an integer exit code
