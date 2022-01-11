// Learn more about F# at http://fsharp.org

open System
open AoC.Utils



let part1ScansCountMerged data =
    let analyzedData = data |> List.map Analysis.analyze
    let resPoints = analyzedData |> List.reduce Analysis.mergePointClouds
    //printfn "  All points: %A" (resPoints |> List.map (fun ae -> ae.Point))
    let res = resPoints |> List.length
    printfn "Part 1, #points: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 19: Beacon Scanner\n======================\n"
    let data = ScannerReadings.loadData (DataInput.SampleI 1)
    //printfn "Data: %A" data
    part1ScansCountMerged data
    0 // return an integer exit code
