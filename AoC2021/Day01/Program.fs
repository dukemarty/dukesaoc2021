// Learn more about F# at http://fsharp.org

open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines @"data\puzzle.txt" |> Seq.map System.Int32.Parse |> List.ofSeq


let detectIncrease a b =
    if (b > a) then
        1
    else
        0

let rec comp a =
    match a with
    | hal::har::ta -> (detectIncrease hal har) :: comp (har::ta)
    //| ha::[] -> []
    | _ -> []

[<EntryPoint>]
let main argv =
    printfn "Day 1: Sonar Sweep\n==================\n"
    //printfn "%A" lines 
    //printfn "%A" (comp lines)
    printfn "Part 1: number of increases = %d" (List.sum (comp lines))
    0 // return an integer exit code
