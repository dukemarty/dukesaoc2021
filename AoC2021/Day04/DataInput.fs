module DataInput

open System
open System.Text.RegularExpressions

let splitListOnElem test lst =
    List.skipWhile
        (fun l -> l = [])
        (List.foldBack
            (fun el acc ->
                match acc with
                | [] when (test el) -> []
                | [] -> [ [ el ] ]
                | xs :: ys when not (test el) -> (el :: xs) :: ys
                | _ -> [] :: acc)
            lst
            [])


let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    readLines @"data\puzzle.txt" |> List.ofSeq

let draws =
    lines.[0].Split(",")
    |> Seq.map Int32.Parse
    |> List.ofSeq

let parseSingleCard (lines: string list) =
    lines |> Seq.map (fun s -> Regex.Split(s.Trim(), "\s+") |> Seq.map Int32.Parse |> List.ofSeq) |> List.ofSeq


let parseCards lines =
    let isEmptyLine s = String.IsNullOrWhiteSpace s
    let cards = splitListOnElem isEmptyLine lines
    cards |> Seq.map parseSingleCard |> List.ofSeq

let cards = parseCards lines.Tail
