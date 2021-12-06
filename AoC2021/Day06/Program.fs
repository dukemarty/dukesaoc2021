// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let initState data =
    seq { for i in 0 .. 8 -> i }
    |> Seq.map (fun i -> (data |> Seq.filter (fun n -> n = i) |> Seq.length))
    |> List.ofSeq

let updateState state =
    let h::t = state
    let t16 = List.take 6 t
    let t7::t8::__ = List.skip 6 t
    let res = t16 @ [ (h + t7) ] @ [ t8 ] @ [ h ]
    res

let rec runDays state days =
    if (days = 0) then
        state
    else
        runDays (updateState state) (days - 1)

let part1Growth80days data =
    let state = initState data
    printfn "  Initial state: %A" state
    let finalState = runDays state 80
    printfn "  Final state: %A" finalState
    let res = finalState |> List.sum
    printfn "Final number of fish after 80 days: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 6: Lanternfish\n==================\n"

    let data =
        DataInput.commaSeparatedIntList DataInput.Puzzle

    printfn "Initial data: %A" (data |> List.ofSeq)
    part1Growth80days data
    0 // return an integer exit code
