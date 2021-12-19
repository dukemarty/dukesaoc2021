// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let part1LongAddition (data: SnailNumber.SnailNumber list) =
    let resSum = data |> List.reduce (fun l r -> SnailNumber.addSnailNumbers l r)
    printfn "  Sum: %O" resSum
    let res = SnailNumber.calcMagnitude resSum
    printfn "Part 1, magnitude of sum: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 18: Snailfish\n=================\n"
    let data = DataParser.loadData (DataInput.Puzzle)
    printfn "Data: %A" data
    part1LongAddition data
    0 // return an integer exit code
