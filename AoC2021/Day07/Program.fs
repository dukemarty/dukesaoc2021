// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1MinFuelLinear (data: int seq) =
    let calcMedian data =
        let sorted = data |> Seq.sort |> List.ofSeq
        let pos = sorted.Length / 2
        sorted.[pos]

    let calcFuel (numFreq: Map<int, int>) keys pivot =
        let sumLeft =
            keys
            |> Seq.filter (fun i -> i < pivot)
            |> Seq.map (fun i -> (pivot - i) * (numFreq.Item i))
            |> Seq.sum

        let sumRight =
            keys
            |> Seq.filter (fun i -> i > pivot)
            |> Seq.map (fun i -> (i - pivot) * (numFreq.Item i))
            |> Seq.sum

        sumLeft + sumRight
    //let initialPivot (keys: int list) (freq: Map<int,int>) =
    //    let index = keys.Length / 2
    //    keys.[index]
    //let rec findOptimalPoint numFreq keys pivot left right =
    //    let fuelPivot = calcFuel numFreq keys pivot
    //    let fuelLeft = calcFuel numFreq keys (pivot-1)
    //    let fuelRight = calcFuel numFreq keys (pivot+1)
    //    if (fuelLeft < fuelPivot) then
    //        findOptimalPoint

    let numFreq = data |> Seq.countBy id |> Map.ofSeq

    let keys =
        numFreq
        |> Seq.map (fun e -> e.Key)
        |> Seq.sort
        |> List.ofSeq

    printfn "  Frequencies: %A" numFreq
    let median = calcMedian data
    let resPoint, resFuel = median, (calcFuel numFreq keys median)
    printfn "Part 1 best point   : %d" resPoint
    printfn "Part 1 fuel required: %d" resFuel

let part2MinFuelIncreasing (data: int seq) =
    let calcFuelForDist d = ((1 + d) * d) / 2

    let calcFuel (numFreq: Map<int, int>) keys pivot =
        let sumLeft =
            keys
            |> Seq.filter (fun i -> i < pivot)
            |> Seq.map (fun i -> calcFuelForDist (pivot - i) * (numFreq.Item i))
            |> Seq.sum

        let sumRight =
            keys
            |> Seq.filter (fun i -> i > pivot)
            |> Seq.map (fun i -> calcFuelForDist (i - pivot) * (numFreq.Item i))
            |> Seq.sum

        sumLeft + sumRight

    let numFreq = data |> Seq.countBy id |> Map.ofSeq

    let keys =
        numFreq
        |> Seq.map (fun e -> e.Key)
        |> Seq.sort
        |> List.ofSeq

    printfn "  Frequencies: %A" numFreq

    let avg =
        Convert.ToInt32(
            round (
                float (data |> Seq.sum)
                / float (data |> List.ofSeq).Length
            )
        )

    printfn "  Average: %d" avg

    let printIntermediateResult (pos, fuel) = 
        printfn "  Result at %d : %d" pos fuel
    let resLeft = avg-1, calcFuel numFreq keys (avg - 1)
    let resAvg = avg, calcFuel numFreq keys avg
    let resRight = avg+1, calcFuel numFreq keys (avg + 1)
    printIntermediateResult resLeft
    printIntermediateResult resAvg
    printIntermediateResult resRight

    let res = seq { resLeft; resAvg; resRight } |> List.ofSeq |> List.minBy (fun (p, f) -> f)
    let resPoint, resFuel = res
    printfn "Part 2 best point   : %d" resPoint
    printfn "Part 2 fuel required: %d" resFuel


[<EntryPoint>]
let main argv =
    printfn "Day 7: The Treachery of Whales\n==============================\n"

    let data =
        DataInput.commaSeparatedIntList DataInput.Sample
    //printfn "Data: %A" (data |> List.ofSeq)
    part1MinFuelLinear data
    part2MinFuelIncreasing data
    0 // return an integer exit code
