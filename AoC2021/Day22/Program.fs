// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let part1HasRelevantParts (step: RebootSteps.RebootStep) =
    ((step.X.From <= 50) && (step.X.To >= -50) || (step.X.From >= 50) && (step.X.To <= -50))
    && ((step.Y.From <= 50) && (step.Y.To >= -50) || (step.Y.From >= 50) && (step.Y.To <= -50) )
    && ((step.Z.From <= 50) && (step.Z.To >= -50) || (step.Z.From >= 50) && (step.Z.To <= -50))

let part1FilterForRelevantSection (step: RebootSteps.RebootStep) =
    let x = { step.X with From=(min step.X.From step.X.To); To=(max step.X.From step.X.To) }
    let xx = { x with From = (max x.From -50); To = (min x.To 50) }
    let y = { step.Y with From=(min step.Y.From step.Y.To); To=(max step.Y.From step.Y.To)}
    let yy = { y with From = (max y.From -50); To = (min y.To 50) }
    let z = { step.Z with From=(min step.Z.From step.Z.To); To=(max step.Z.From step.Z.To)}
    let zz = { z with From = (max z.From -50); To = (min z.To 50) }

    { step with X = xx; Y = yy; Z = zz }

let stepToCuboids (step: RebootSteps.RebootStep) =
    let res =
        seq { for x in step.X.From .. step.X.To -> x }
        |> Seq.collect
            (fun x ->
                seq { for y in step.Y.From .. step.Y.To -> y }
                |> Seq.collect
                    (fun y ->
                        seq { for z in step.Z.From .. step.Z.To -> z }
                        |> Seq.map (fun z -> (x, y, z))))
        |> Set.ofSeq
    //printfn "  Created cuboids for (%O): %A" step res
    res

let part1CountTurnedOnCubes data =
    let applyStep onSet (step: RebootSteps.RebootStep) =
        let res = match step.Action with
                  | RebootSteps.On -> Set.union onSet (stepToCuboids step)
                  | RebootSteps.Off -> Set.difference onSet (stepToCuboids step)
        printfn "  Set after applying (%d..%d, %d..%d, %d..%d): %d" step.X.From step.X.To step.Y.From step.Y.To step.Z.From step.Z.To res.Count
        res
    let focussedData =
        data
        |> Seq.filter part1HasRelevantParts
        |> Seq.map part1FilterForRelevantSection

    printfn "  Focussed data: %A" (focussedData |> List.ofSeq)
    let onSet = focussedData |> Seq.fold applyStep Set.empty
    let res = onSet.Count
    printfn "Part 1, #cubes=on: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 22: Reactor Reboot\n======================\n"

    let data =
        RebootSteps.loadAllSteps (DataInput.Puzzle)

    //printfn "Data: %A" data
    part1CountTurnedOnCubes data
    0 // return an integer exit code
