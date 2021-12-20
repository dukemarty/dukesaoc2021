// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

[<EntryPoint>]
let main argv =
    printfn "Day 19: Beacon Scanner\n======================\n"
    let data = ScannerReadings.loadData DataInput.Sample
    printfn "Data: %A" data
    0 // return an integer exit code
