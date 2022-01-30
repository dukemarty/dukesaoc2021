namespace AoC.Utils

open System

module DataInput =

    type DataSource =
        | Sample
        | Puzzle
        | SampleI of int

    let private readLines filePath = System.IO.File.ReadLines(filePath)

    let standardSample s =
        let file = @$"data\sample{s}.txt"
        readLines file |> List.ofSeq

    let standardPuzzle =
        readLines @"data\puzzle.txt" |> List.ofSeq

    let multipleRawLines source =
        match source with
        | Sample -> standardSample ""
        | Puzzle -> standardPuzzle
        | SampleI i -> standardSample (i.ToString())

    let singleRawLine source =
        (match source with
         | Sample -> standardSample ""
         | Puzzle -> standardPuzzle
         | SampleI i -> standardSample (i.ToString()))
            .Head

    let commaSeparatedIntList source =
        (singleRawLine source).Split(',')
        |> Seq.map Int32.Parse

    let intMatrix source =
        multipleRawLines source
        |> Seq.map
            (fun r ->
                r.ToCharArray()
                |> Seq.map (fun c -> Int32.Parse(string c))
                |> Array.ofSeq)
        |> Array.ofSeq
