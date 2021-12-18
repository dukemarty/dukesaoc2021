// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let stepsToX x = -0.5 + sqrt (0.25 + 2.0 * (float x))

let generalGauss m n = ((m + n) * (n - m + 1)) / 2

let isInRange lbound ubound n = (lbound <= n) && (n <= ubound)

let determineXrangePart1 (target: TargetArea.TargetArea) =
    let minVal =
        int (Math.Ceiling(stepsToX target.Xfrom))

    let maxVal =
        seq { for i in minVal .. target.Xto -> i }
        |> Seq.filter (isInRange target.Xfrom target.Xto)
        |> Seq.max

    minVal, maxVal

let checkYPossibleForSteps (target: TargetArea.TargetArea) minSteps maxSteps =
    let hitCheckHelper y =
        let stepsToZeroAgain = 2 * y + 1
        let m = y + 1

        not (
            (seq { for i in minSteps .. maxSteps -> i }
             |> Seq.map (fun s -> (s - stepsToZeroAgain))
             |> Seq.map (fun (rs: int) -> generalGauss m (m + rs))
             |> Seq.map (fun d -> (target.Yfrom <= -d) && (-d <= target.Yto))
             |> Seq.filter id
             |> List.ofSeq)
                .IsEmpty
        )


    seq { for i in 1 .. (abs target.Yfrom) -> i }
    |> Seq.filter hitCheckHelper


let part1highestY target =
    let minSteps, maxSteps = determineXrangePart1 target
    printfn "  Range of steps: [%d, %d]" minSteps maxSteps

    let yCands =
        checkYPossibleForSteps target minSteps maxSteps

    printfn "  Candidates for y: %A" yCands
    let yForMaxHeight = yCands |> Seq.maxBy (generalGauss 1)
    let res = generalGauss 1 yForMaxHeight
    printfn "Part 1, highest reachable point: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 17: Trick Shot\n==================\n"
    let target = TargetArea.loadData DataInput.Puzzle
    printfn "Target Area: %O" target
    part1highestY target
    0 // return an integer exit code
