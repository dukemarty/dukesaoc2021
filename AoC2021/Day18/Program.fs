// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let part1LongAddition (data: SnailNumber.SnailNumber list) =
    let resSum =
        data
        |> List.reduce (fun l r -> SnailNumber.addSnailNumbers l r)

    printfn "  Sum: %O" resSum
    let res = SnailNumber.calcMagnitude resSum
    printfn "Part 1, magnitude of sum: %d" res


let part2LargestMagnitude (data: SnailNumber.SnailNumber list) =
    let rec helper remNums largest =
        match remNums with
        | [ x ] -> largest
        | x :: xs ->
            let cand =
                xs
                |> Seq.collect
                    (fun sn ->
                        [ SnailNumber.calcMagnitude (SnailNumber.addSnailNumbers x sn)
                          SnailNumber.calcMagnitude (SnailNumber.addSnailNumbers sn x) ])
                |> Seq.max

            helper xs (max largest cand)

    let res = helper data 0
    printfn "Part 2, largest magnitude: %d" res


[<EntryPoint>]
let main argv =
    printfn "Day 18: Snailfish\n=================\n"
    let data = DataParser.loadData (DataInput.Puzzle)
    printfn "Data: %A" data
    part1LongAddition data
    part2LargestMagnitude data
    0 // return an integer exit code
