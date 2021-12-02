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

let rec divePos data horiz depth =
    match data with
    | (Forward v) :: taildata -> divePos taildata (horiz + v) depth
    | (Up v) :: taildata -> divePos taildata horiz (depth - v)
    | (Down v) :: taildata -> divePos taildata horiz (depth + v)
    | [] -> (horiz, depth)

let part1 data =
    let horiz, depth = divePos data 0 0
    printfn "Part 1: %d, %d -> %d" horiz depth (horiz * depth)
    0


[<EntryPoint>]
let main argv =
    printfn "Day 2: Dive!\n============\n"
    //printfn "%A" lines
    part1 lines
    0 // return an integer exit code
