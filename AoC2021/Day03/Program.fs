// Learn more about F# at http://fsharp.org

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    readLines @"data\puzzle.txt" |> List.ofSeq

// sum up bit values on position pos of a set of binary-encoded number strings
let countSingleBitsOnPos numberList pos =
    let extractChar (numberStr: string) = string (numberStr.[pos])

    numberList
    |> Seq.map extractChar
    |> Seq.map Int32.Parse
    |> Seq.sum

let part1PowerConsumption (lines: string list) =
    let numberLen = (List.head lines).Length

    let singleBits =
        seq { for i in 0 .. numberLen - 1 -> i }
        |> Seq.map (countSingleBitsOnPos lines)

    let gammaBits =
        let convertGammaCount max cnt = if (cnt > max - cnt) then "1" else "0"

        singleBits
        |> Seq.map (convertGammaCount lines.Length)

    let gamma =
        Convert.ToInt32((String.concat "" gammaBits), 2)

    let epsilonBits =
        let convertEpsilonCount max cnt = if (cnt > max - cnt) then "0" else "1"

        singleBits
        |> Seq.map (convertEpsilonCount lines.Length)

    let epsilon =
        Convert.ToInt32((String.concat "" epsilonBits), 2)

    printfn "%A" singleBits
    printfn "Power Consumption: %d (%d * %d)" (gamma * epsilon) gamma epsilon


let rec findSingleForRating (lines: string list, pos, criterion) =
    let numberLen = (List.head lines).Length

    let singleBits =
        seq { for i in 0 .. numberLen - 1 -> i }
        |> Seq.map (countSingleBitsOnPos lines)

    let criterionBits =
        singleBits
        |> Seq.map (criterion lines.Length)
        |> List.ofSeq

    let matchOnPos pos bit (value: string) = string (value.[pos]) = bit

    match lines with
    | h :: [] -> h
    | [] -> "0"
    | _ ->
        findSingleForRating (
            lines
            |> List.filter (matchOnPos pos criterionBits.[pos]),
            (pos + 1),
            criterion
        )

let part2LifeSupportRating (lines: string list) =

    let oxygenCriterion max cnt = if (cnt >= max - cnt) then "1" else "0"

    let co2Criterion max cnt = if (cnt >= max - cnt) then "0" else "1"

    let oxygenGeneratorRating =
        Convert.ToInt32(findSingleForRating (lines, 0, oxygenCriterion), 2)

    let co2ScrubberRating =
        Convert.ToInt32(findSingleForRating (lines, 0, co2Criterion), 2)

    printfn
        "Life support rating: %d (%d * %d)"
        (oxygenGeneratorRating * co2ScrubberRating)
        oxygenGeneratorRating
        co2ScrubberRating


[<EntryPoint>]
let main argv =
    printfn "Day 3: Binary Diagnostic\n========================\n"
    part1PowerConsumption lines
    part2LifeSupportRating lines
    0 // return an integer exit code
