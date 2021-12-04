// Learn more about F# at http://fsharp.org

open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines @"data\puzzle.txt" |> List.ofSeq

[<EntryPoint>]
let main argv =
    printfn "Day 4: Giant Squid\n==================\n"
    0 // return an integer exit code
