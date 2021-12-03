// Learn more about F# at http://fsharp.org

open System

type Command =
    | None
    | Forward of int
    | Up of int
    | Down of int


let parseLine (l: string) =
    let tokens = l.Split [| ' ' |] |> Array.toList

    match tokens with
    | "forward" :: v :: [] -> Forward(System.Int32.Parse v)
    | "up" :: v :: [] -> Up(System.Int32.Parse v)
    | "down" :: v :: [] -> Down(System.Int32.Parse v)
    | _ -> None

let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    readLines @"data\puzzle.txt"
    |> Seq.map parseLine
    |> List.ofSeq

let rec divePosPart1 data horiz depth =
    match data with
    | (Forward v) :: taildata -> divePosPart1 taildata (horiz + v) depth
    | (Up v) :: taildata -> divePosPart1 taildata horiz (depth - v)
    | (Down v) :: taildata -> divePosPart1 taildata horiz (depth + v)
    | [] -> (horiz, depth)

let part1 data =
    let horiz, depth = divePosPart1 data 0 0
    printfn "Part 1: %d, %d -> %d" horiz depth (horiz * depth)

let rec divePosPart2 data horiz depth aim =
    match data with
        | (Forward v) :: taildata -> divePosPart2 taildata (horiz + v) (depth + v*aim) aim
        | (Up v) :: taildata -> divePosPart2 taildata horiz depth (aim - v)
        | (Down v) :: taildata -> divePosPart2 taildata horiz depth (aim + v)
        | [] -> (horiz, depth, aim)

let part2 data =
    let horiz, depth, aim = divePosPart2 data 0 0 0
    printfn "Part 2: %d, %d (%d) -> %d" horiz depth aim (horiz*depth)

[<EntryPoint>]
let main argv =
    printfn "Day 2: Dive!\n============\n"
    //printfn "%A" lines
    part1 lines
    part2 lines
    0 // return an integer exit code
