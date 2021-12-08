// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

type SignalMapping = { orig: char; image: char }
type DigitMapping = { orig: int; image: string }

type DigitHypothesis =
    { orig: int
      candidates: (char array) list }

type SignalHypothesis =
    { orig: char
      candidates: (char array) list }

let buildInitialHypotheses (dig: char list array) (data: DisplayData.InputRecord) =
    seq { for i in 0 .. 9 -> i }
    |> Seq.map (fun i -> { DigitHypothesis.orig = i; candidates = (data.Signals |> Seq.filter (fun sa -> sa.Length=dig.[i].Length) |> List.ofSeq) })

let part1SingleDisplay dig (data: DisplayData.InputRecord) =
    let hyp = buildInitialHypotheses dig data
    let mapping = hyp |> Seq.filter (fun h -> h.candidates.Length = 1) |> Seq.map (fun h -> { DigitMapping.orig = h.orig; image = h.candidates.Head |> System.String })
    //printfn "  Mappings: %A" mapping
    let wantedDigits = [| 1; 4; 7; 8|] |> Set.ofArray
    let foundCodes = mapping |> Seq.filter (fun m -> wantedDigits.Contains m.orig ) |> Seq.map (fun m -> m.image) |> Set.ofSeq
    printfn "  Data codes : %A" data.Codes
    printfn "  Found codes: %A" foundCodes
    data.Codes |> Seq.filter (fun c -> foundCodes.Contains c) |> Seq.length

let part1Identify1478 dig data =
    let partResults = data |> Seq.map (part1SingleDisplay dig)
    let res = partResults |> Seq.sum
    printfn "Count of 1,4,7,8: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 8: Seven Segment Search\n===========================\n"

    let data = DisplayData.inputData DataInput.Puzzle
    let digits = DisplayData.digitStructure
    part1Identify1478 digits data
    0 // return an integer exit code
