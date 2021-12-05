namespace AoC.Utils

module DataInput =

    let private readLines filePath = System.IO.File.ReadLines(filePath)

    let standardSample =
        readLines @"data\sample.txt" |> List.ofSeq


    let standardPuzzle =
        readLines @"data\puzzle.txt" |> List.ofSeq


module DataProcessing =

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
    