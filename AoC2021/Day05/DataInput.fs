module DataInput

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let rawLines =
    readLines @"data\puzzle.txt" |> List.ofSeq

let parseLine (line: string) =
    line.Split(" -> ")
    |> Seq.map
        (fun s ->
            s.Trim().Split(",")
            |> Seq.map Int32.Parse
            |> List.ofSeq)
    |> List.ofSeq

let lines = rawLines |> Seq.map parseLine
