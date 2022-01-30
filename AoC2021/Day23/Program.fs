// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let part1CheapestSolution (state: BurrowState.Amphipod list) =
    let solution = Search.runAStarSearch state
    printfn "  solution node: %O" solution
    printfn "Part 1, cost of cheapest solution: %d" solution.AccumulatedCost

[<EntryPoint>]
let main argv =
    printfn "Day 23: Amphipod\n================\n"
    let state = BurrowState.loadInitialState DataInput.Sample
    printfn "Data:\n  %A" state
    part1CheapestSolution state
    0 // return an integer exit code
