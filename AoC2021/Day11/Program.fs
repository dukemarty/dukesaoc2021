// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let performStep config =
    let allPos = seq { for r in 0..9 -> seq { for c in 0..9 -> (r, c) } } |> Seq.concat
    let createNeighbours pos =
        let r, c = pos
        let cands =
            [| (r, c-1)
               (r-1, c-1)
               (r-1, c)
               (r-1, c+1)
               (r, c+1)
               (r+1, c+1)
               (r+1, c)
               (r+1, c-1) |]
        cands |> Seq.filter (fun (x, y) -> (x>=0) && (y>=0) && (x<10) && (y<10))

    let incrementConfig config = 
        config |> Array.map (fun row -> row |> Array.map (fun i -> i + 1))
    let applyFlashes (config: int array array) (flashes: Set<int*int>) =
        let bleedPoints = flashes |> Seq.map createNeighbours |> Seq.concat
        //|> Seq.map (fun ns -> ns |> Seq.map (fun (r, c) -> (config.[r].[c] <- (config.[r].[c] + 1))))
        //|> ignore
        for (r, c) in bleedPoints do
            config.[r].[c] <- config.[r].[c] + 1
        config
    let rec performFlashes flashes (config: int array array) =
        //let allPos = seq { for r in 0..9 -> seq { for c in 0..9 -> (r, c) } } |> Seq.concat
        let flashPoints = allPos |> Seq.filter (fun (r, c) -> config.[r].[c] > 9) |> Set.ofSeq
        let newFlashPoints = Set.difference flashPoints flashes
        match (newFlashPoints |> Set.toList) with
        | [] -> config, flashes.Count
        | _ -> performFlashes (Set.union flashes newFlashPoints) (applyFlashes config newFlashPoints)
    let resetFlashedCells (config: int array array) =
        //allPos |> Seq.map (fun (r, c) -> if (config.[r].[c] >= 9) then (config.[r].[c] <- 0)) |> ignore
        let resetPoints = allPos |> Seq.filter (fun (r, c) -> config.[r].[c] > 9)
        for (r, c) in resetPoints do
            config.[r].[c] <- 0
        config
    let incrementedConfig = config |> incrementConfig
    //printfn "  Incremented: %A" incrementedConfig
    let flashedConfig, flashCount = incrementedConfig |> performFlashes Set.empty
    let resetConfig = resetFlashedCells flashedConfig
    resetConfig, flashCount

let runConfigPart1 startConfig steps =
    let rec runConfigHelper config remainingSteps flashCount =
        match remainingSteps with
        | 0 -> config, flashCount
        | _ -> 
            let newConfig, additionalFlashes = performStep config
            runConfigHelper newConfig (remainingSteps - 1) (flashCount + additionalFlashes)
    runConfigHelper startConfig steps 0

let part1FlashesIn100Steps data =
    let endConfig, flashCount = runConfigPart1 data 100
    printfn "Part 1, #flashes: %d" flashCount

let part2FullFlash data =
    let rec part2Helper data i =
        let config, _ = performStep data
        let sum = config |> Array.map (fun row -> row |> Array.sum) |> Array.sum
        if (sum = 0) then
            i
        else
            part2Helper config (i+1)
    let res = part2Helper data 1
    printfn "Part 2, #steps: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 11: Dumbo Octopus\n=====================\n"
    let data = DataInput.intMatrix DataInput.Puzzle
    printfn "Data: %A" data
    part1FlashesIn100Steps data
    let data = DataInput.intMatrix DataInput.Puzzle
    part2FullFlash data
    0 // return an integer exit code
