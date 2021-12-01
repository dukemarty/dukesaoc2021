// Learn more about F# at http://fsharp.org

open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines @"data\puzzle.txt" |> Seq.map System.Int32.Parse |> List.ofSeq


let detectIncreaseWin1 a b =
    if (b > a) then
        1
    else
        0

let rec part1Windowing a =
    match a with
    | hal::har::ta -> (detectIncreaseWin1 hal har) :: part1Windowing (har::ta)
    | _ -> []

let detectIncreaseWin3 a b c d =
    if (b+c+d > a+b+c) then
        1
    else
        0

let rec part2Windowing a =
    match a with
    | ha1::ha2::ha3::ha4::ta -> (detectIncreaseWin3 ha1 ha2 ha3 ha4) :: part2Windowing (ha2::ha3::ha4::ta)
    | _ -> []

[<EntryPoint>]
let main argv =
    printfn "Day 1: Sonar Sweep\n==================\n"
    //printfn "%A" lines 
    //printfn "%A" (comp lines)
    printfn "Part 1: number of increases = %d" (List.sum (part1Windowing lines))
    printfn "Part 2: number of increases = %d" (List.sum (part2Windowing lines))
    0 // return an integer exit code
