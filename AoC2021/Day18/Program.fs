// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

[<EntryPoint>]
let main argv =
    printfn "Day 18: Snailfish\n=================\n"
    let data = DataParser.loadData DataInput.Sample
    printf "Data: %A" data
    0 // return an integer exit code
