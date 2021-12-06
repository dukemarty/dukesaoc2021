// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1Growth80days =
    let res = 0
    printfn "Final number of fish after 80 days: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 6: Lanternfish\n==================\n"
    let initState = DataInput.commaSeparatedIntList DataInput.Sample
    printfn "Initial data: %A" (initState |> List.ofSeq)
    part1Growth80days
    0 // return an integer exit code
