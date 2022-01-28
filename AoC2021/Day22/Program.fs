// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1CountTurnedOnCubes data =
    let applyStep onSet (step: RebootSteps.RebootStep) =
        let res = match step.Action with
                  | RebootSteps.On -> Set.union onSet (Part1.stepToCuboids step)
                  | RebootSteps.Off -> Set.difference onSet (Part1.stepToCuboids step)
        printfn "  Set after applying (%d..%d, %d..%d, %d..%d): %d" step.X.From step.X.To step.Y.From step.Y.To step.Z.From step.Z.To res.Count
        res
    let focussedData =
        data
        |> Seq.filter Part1.hasRelevantParts
        |> Seq.map Part1.filterForRelevantSection

    printfn "  Focussed data: %A" (focussedData |> List.ofSeq)
    let onSet = focussedData |> Seq.fold applyStep Set.empty
    let res = onSet.Count
    printfn "Part 1, #cubes=on: %d" res

let part2ApplyRebootStepV1 l (rs: RebootSteps.RebootStep): Cuboid.Cuboid list =
    match rs with
    | r when r.Action=RebootSteps.On ->
        let rCuboid = RebootSteps.getCuboid r
        let addOn = l |> List.fold (fun sl t -> 
                                        sl |> List.collect (fun s ->
                                            let rel = Cuboid.determineRelations s t
                                            if (Cuboid.areSomehowRelated rel) then
                                                let res, _ = Cuboid.splitOnAxes s t rel
                                                res
                                            else
                                                [s])
                                        ) [rCuboid]
        printf "Split %A to %A" rCuboid addOn
        List.concat [addOn; l]
    | r when r.Action=RebootSteps.Off ->
        let rCuboid = RebootSteps.getCuboid r
        l |> List.collect (fun c ->
                                let rel = Cuboid.determineRelations c rCuboid
                                if (Cuboid.areSomehowRelated rel) then
                                    let unrelated, _ = Cuboid.splitOnAxes c rCuboid rel
                                    unrelated
                                else
                                    [c])

let part2CountTurnedOnCubesV1 data =
    let mutable i = 0
    let resOn = data |> List.fold (fun acc rs -> 
                                        printfn "Applying %d on %d cuboids..." i (List.length acc)
                                        i <- i + 1
                                        part2ApplyRebootStepV1 acc rs) []
    let res: int64 = resOn |> List.map Cuboid.calcVolume |> List.reduce (+)
    printfn "Part 2, #cubes=on: %d" res

let part2ApplyRebootStepV2 l (rs: RebootSteps.RebootStep): Cuboid.Cuboid list =
    match rs with
    | r when r.Action=RebootSteps.On ->
        let rCuboid = RebootSteps.getCuboid r
        rCuboid :: l
    | r when r.Action=RebootSteps.Off ->
        let rCuboid = RebootSteps.getCuboid r
        l |> List.collect (fun c ->
                                let rel = Cuboid.determineRelations c rCuboid
                                if (Cuboid.areSomehowRelated rel) then
                                    let unrelated, _ = Cuboid.splitOnAxes c rCuboid rel
                                    unrelated
                                else
                                    [c])


let part2CountTurnedOnCubesV2 data =
    let calcAddonValue currentCub seenCubsList =
        let filteredCubs = seenCubsList
                           |> List.fold (fun sl seenCub ->
                                            sl
                                            |> List.collect (fun s ->
                                                                let rel = Cuboid.determineRelations s seenCub
                                                                if (Cuboid.areSomehowRelated rel) then
                                                                    let u, _ = Cuboid.splitOnAxes s seenCub rel
                                                                    u
                                                                else
                                                                    [s])
                                            ) [currentCub]
        if (filteredCubs |> List.length = 0) then
            0L
        else 
            let res = filteredCubs |> List.map Cuboid.calcVolume |> List.reduce (+)
            res

    let mutable i = 0
    let resOn = data |> List.fold (fun acc rs -> 
                                        printfn "Applying %d on %d cuboids..." i (List.length acc)
                                        i <- i + 1
                                        part2ApplyRebootStepV2 acc rs) []
    let addValues = resOn |> List.mapi (fun i c -> calcAddonValue c (List.take i resOn))
    //let addValues = resOn |> List.mapi (fun i c -> calcAddonValue c (List.take (max 0 (i)) resOn))
    //let res: int64 = resOn |> List.map Cuboid.calcVolume |> List.reduce (+)
    let res = addValues |> List.reduce (+)
    printfn "Part 2, #cubes=on: %d" res


[<EntryPoint>]
let main argv =
    printfn "Day 22: Reactor Reboot\n======================\n"

    let data =
        RebootSteps.loadAllSteps (DataInput.Puzzle)
    //printfn "Data: %A" data

    //part1CountTurnedOnCubes data
    part2CountTurnedOnCubesV2 data
    0 // return an integer exit code
