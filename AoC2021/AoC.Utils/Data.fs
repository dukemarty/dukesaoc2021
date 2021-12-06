namespace AoC.Utils

open System

module DataInput =

    type DataSource =
        | Sample
        | Puzzle

    let private readLines filePath = System.IO.File.ReadLines(filePath)

    let standardSample =
        readLines @"data\sample.txt" |> List.ofSeq

    let standardPuzzle =
        readLines @"data\puzzle.txt" |> List.ofSeq

    let multipleRawLines source =
        match source with
        | Sample -> standardSample
        | Puzzle -> standardPuzzle

    let singleRawLine source =
        (match source with
         | Sample -> standardSample
         | Puzzle -> standardPuzzle)
            .Head

    let commaSeparatedIntList source =
        (singleRawLine source).Split(",")
        |> Seq.map Int32.Parse
