// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let isLowPoint data r c =
    let w, h = Heatmap.getWidthHeight data
    let cands = [| (c-1, r); (c+1,r); (c,r-1); (c,r+1) |]
    let neighbours = cands |> Seq.filter (fun (x, y) -> (x >= 0) && (y >=0) && (x < w) && (y < h))
    //printfn "Neighbour positions for (%d, %d): %A" r
    let countLowerEqualNeighbours = neighbours |> Seq.map (fun (x, y) -> if data.[r].[c] < data.[y].[x] then 0 else 1) |> Seq.sum
    countLowerEqualNeighbours = 0

let findLowPoints data =
    let w, h = Heatmap.getWidthHeight data
    seq { for i in 0..(h-1) -> i} |> Seq.map (fun r -> seq { for j in 0..(w-1) -> j} |> Seq.filter (fun c -> isLowPoint data r c) |> Seq.map (fun c -> (r, c))) |> Seq.concat

let part1RisklevelSum data =
    let lowPoints = findLowPoints data |> List.ofSeq
    //printfn "  Low points : %A" lowPoints
    let riskLevels = lowPoints |> Seq.map (fun (r, c) -> data.[r].[c] + 1) |> List.ofSeq
    //printfn "  Risk levels: %A" riskLevels
    let res = riskLevels |> Seq.sum
    printfn "Part 1, sum of risk levels: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 9: Smoke Basin\n==================\n"
    let data = Heatmap.loadHeatmap DataInput.Puzzle
    part1RisklevelSum data
    0 // return an integer exit code
