// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let buildInitialHypotheses (dig: char list array) (data: DisplayData.InputRecord) =
    seq { for i in 0 .. 9 -> i }
    |> Seq.map
        (fun i ->
            Analysis.createDigitHypothesis
                i
                (data.Signals
                 |> Seq.filter (fun sa -> sa.Length = dig.[i].Length)
                 |> List.ofSeq))
    |> List.ofSeq

let part1SingleDisplay dig (data: DisplayData.InputRecord) =
    let hyp = buildInitialHypotheses dig data

    let mapping =
        hyp
        |> Seq.filter (fun h -> h.Candidates.Length = 1)
        |> Seq.map (fun h -> Analysis.createDigitMapping h.Orig (h.Candidates.Head |> System.String))
    //printfn "  Mappings: %A" mapping
    let wantedDigits = [| 1; 4; 7; 8 |] |> Set.ofArray

    let foundCodes =
        mapping
        |> Seq.filter (fun m -> wantedDigits.Contains m.Orig)
        |> Seq.map (fun m -> m.Image)
        |> Set.ofSeq
    //printfn "  Data codes : %A" data.Codes
    //printfn "  Found codes: %A" foundCodes
    data.Codes
    |> Seq.filter (fun c -> foundCodes.Contains c)
    |> Seq.length

let part1Identify1478 dig data =
    let partResults = data |> Seq.map (part1SingleDisplay dig)
    let res = partResults |> Seq.sum
    printfn "Part 1, count of 1,4,7,8: %d" res

let calcValue map (data: DisplayData.InputRecord) =
    let digits = data.Codes |> Seq.map (fun s -> Analysis.decode map s)
    let stringVal = digits |> Seq.map (fun i -> (string i).[0]) |> Array.ofSeq |> System.String
    Int32.Parse stringVal

let part2SingleDisplay dig (data: DisplayData.InputRecord) =
    let startHyp = buildInitialHypotheses dig data

    let baseDigitMapping =
        startHyp
        |> Seq.filter (fun h -> h.Candidates.Length = 1)
        |> Seq.map
            (fun h ->
                { Analysis.DigitMapping.Orig = h.Orig
                  Analysis.DigitMapping.Image = h.Candidates.Head |> System.String })

    let digitMapping =
        Analysis.identifyMissingMappings startHyp baseDigitMapping


    ////printfn "  Mappings: %A" mapping
    //let wantedDigits = [| 1; 4; 7; 8|] |> Set.ofArray
    //let foundCodes = mapping |> Seq.filter (fun m -> wantedDigits.Contains m.orig ) |> Seq.map (fun m -> m.image) |> Set.ofSeq


    //printfn "  Data codes : %A" data.Codes
    //printfn "  Found codes: %A" foundCodes
    //data.Codes |> Seq.filter (fun c -> foundCodes.Contains c) |> Seq.length
    calcValue digitMapping data

let part2IdentifyAllAndSum dig data =
    let partResults = data |> Seq.map (part2SingleDisplay dig)
    let res = partResults |> Seq.sum
    printfn "Part 2, sum of of decoded numbers: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 8: Seven Segment Search\n===========================\n"

    let data = DisplayData.inputData DataInput.Puzzle
    let digits = DisplayData.digitStructure
    part1Identify1478 digits data
    part2IdentifyAllAndSum digits data
    0 // return an integer exit code
