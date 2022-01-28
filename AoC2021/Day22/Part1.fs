
module Part1

let hasRelevantParts (step: RebootSteps.RebootStep) =
    ((step.X.From <= 50) && (step.X.To >= -50) || (step.X.From >= 50) && (step.X.To <= -50))
    && ((step.Y.From <= 50) && (step.Y.To >= -50) || (step.Y.From >= 50) && (step.Y.To <= -50) )
    && ((step.Z.From <= 50) && (step.Z.To >= -50) || (step.Z.From >= 50) && (step.Z.To <= -50))

let filterForRelevantSection (step: RebootSteps.RebootStep) =
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
