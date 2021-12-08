module DisplayData

open System
open AoC.Utils

let digitStructure =
    [| "abcefg"
       "cf"
       "acdeg"
       "acdfg"
       "bcdf"
       "abdfg"
       "abdefg"
       "acf"
       "abcdefg"
       "abcdfg" |]
    |> Seq.map (fun s -> s.ToCharArray() |> Seq.sort |> List.ofSeq) |> Array.ofSeq

let parseSignals (s: string) =
    s.Split(" ")
    |> Seq.map (fun t -> t.Trim().ToCharArray() |> Array.sort)
    |> List.ofSeq

let parseCodes (s: string) =
    s.Split(" ") |> Seq.map (fun t -> t.Trim().ToCharArray() |> Array.sort |> System.String) |> Array.ofSeq


type InputRecord = {
    Signals: (char array) list
    Codes: string array
}

let private parseInputRecord (line: string) =
    let signals :: code :: _ = line.Split(" | ") |> List.ofArray
    { Signals = parseSignals signals ; Codes = parseCodes code }

let private parseInput lines = lines |> Seq.map parseInputRecord

let inputData source =
    parseInput (DataInput.multipleRawLines source)
