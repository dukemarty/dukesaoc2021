module OrigamiData

open System
open System.Text.RegularExpressions
open AoC.Utils


type DotPos = { X: int; Y: int }

type FoldDirection =
    | X
    | Y

type Fold = { Dir: FoldDirection; FoldPos: int }

let createFold dir pos =
    match dir with
    | "x" -> { Dir = FoldDirection.X; FoldPos = pos }
    | "y" -> { Dir = FoldDirection.Y; FoldPos = pos }
    | _ -> failwith "Can not parse fold direction"

let private rawData source =
    DataInput.multipleRawLines source
    |> DataProcessing.splitListOnElem DataProcessing.isEmptyLine

let private parseDots (data: string list) =
    data
    |> Seq.map
        (fun s ->
            match (List.ofArray (s.Split(","))) with
            | x :: y :: _ ->
                { X = (Int32.Parse(x.Trim()))
                  Y = (Int32.Parse(y.Trim())) }
            | _ -> failwith "")
    |> List.ofSeq

let private parseFolds (data: string list) =
    let parseSingleFold line =
        let m =
            Regex.Match(line, "fold along ([xy])=(\d+)")

        createFold (m.Groups.[1].Value) (Int32.Parse(m.Groups.[2].Value.Trim()))

    data |> Seq.map parseSingleFold |> List.ofSeq

let loadData source =
    let data = rawData source

    match data with
    | rawDots :: rawFolds :: _ -> (parseDots rawDots), (parseFolds rawFolds)
    | _ -> failwith "Input data has incorrect format (not enough data sections)"
