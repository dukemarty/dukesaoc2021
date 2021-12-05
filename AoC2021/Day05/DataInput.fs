module DataInput

open System
open AoC.Utils

let parseLine (line: string) =
    line.Split(" -> ")
    |> Seq.map
        (fun s ->
            s.Trim().Split(",")
            |> Seq.map Int32.Parse
            |> List.ofSeq)
    |> List.ofSeq

let lines = DataInput.standardPuzzle |> Seq.map parseLine
//let lines = DataInput.standardSample |> Seq.map parseLine
