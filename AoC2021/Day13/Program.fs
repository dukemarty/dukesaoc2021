// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let applyFold (d: Set<OrigamiData.DotPos>) (f: OrigamiData.Fold) =
    match f.Dir with
    | dir when dir = OrigamiData.FoldDirection.X ->
        d
        |> Set.map
            (fun dp ->
                if (dp.X > f.FoldPos) then
                    { dp with
                          X = (f.FoldPos - (dp.X - f.FoldPos)) }
                else
                    dp)
    | dir when dir = OrigamiData.FoldDirection.Y ->
        d
        |> Set.map
            (fun dp ->
                if (dp.Y > f.FoldPos) then
                    { dp with
                          Y = (f.FoldPos - (dp.Y - f.FoldPos)) }
                else
                    dp)


let part1CountVisibleDots dots (folds: OrigamiData.Fold list) =
    let dotsAfterOneStep =
        Seq.fold (applyFold) (Set.ofList dots) [ folds.Head ]

    let res = dotsAfterOneStep.Count
    printfn "Part 1, #dots: %d" res


let part2PrintDecoded dots (folds: OrigamiData.Fold list) =
    let finalDots =
        Seq.fold (applyFold) (Set.ofList dots) folds

    let outWidth, outHeight =
        (finalDots
         |> Set.map (fun d -> d.X)
         |> Set.maxElement),
        (finalDots
         |> Set.map (fun d -> d.Y)
         |> Set.maxElement)

    let displayDots =
        seq { for i in 0 .. (outHeight + 1) -> i }
        |> Seq.map (fun _ -> Array.create (outWidth + 1) ".")
        |> Array.ofSeq

    for d in finalDots do
        displayDots.[d.Y].[d.X] <- "#"

    let display =
        String.Join(
            "\n",
            (displayDots
             |> Array.map (fun r -> String.Join("", r)))
        )

    let res = finalDots.Count
    printfn "Part 2, decoded display:\n%s" display


[<EntryPoint>]
let main argv =
    printfn "Day 13: Transparent Origami\n===========================\n"
    let dots, folds = OrigamiData.loadData DataInput.Puzzle
    //printfn "Data: %A" dots
    //printfn "Folds: %A" folds
    part1CountVisibleDots dots folds
    part2PrintDecoded dots folds
    0 // return an integer exit code
