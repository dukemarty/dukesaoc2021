// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1CountVisibleDots dots (folds: OrigamiData.Fold list) =
    let applyFold (d: Set<OrigamiData.DotPos>) (f: OrigamiData.Fold) =
        match f.Dir with
        | dir when dir = OrigamiData.FoldDirection.X -> d |> Set.map (fun dp -> if (dp.X > f.FoldPos) then { dp with X=(f.FoldPos - (dp.X - f.FoldPos)) } else dp)
        | dir when dir = OrigamiData.FoldDirection.Y -> d |> Set.map (fun dp -> if (dp.Y > f.FoldPos) then { dp with Y=(f.FoldPos - (dp.Y - f.FoldPos)) } else dp)

    let dotsAfterOneStep =
        Seq.fold (applyFold) (Set.ofList dots) [folds.Head]

    let res = dotsAfterOneStep.Count
    printfn "Part 1, #dots: %d" res


[<EntryPoint>]
let main argv =
    printfn "Day 13: Transparent Origami\n===========================\n"
    let dots, folds = OrigamiData.loadData DataInput.Puzzle
    printfn "Data: %A" dots
    printfn "Folds: %A" folds
    part1CountVisibleDots dots folds
    0 // return an integer exit code
