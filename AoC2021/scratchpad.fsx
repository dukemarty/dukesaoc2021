let s = seq { for i in 1 .. 10 -> i}


let extractChar pos (numberStr: string) =
    numberStr.[pos]

extractChar 2 "00100"

let countSingleBit numberList pos =
    numberList |> Seq.map (extractChar pos) |> Seq.map Int32.Parse |> Seq.sum
    

let part1PowerConsumption (lines: string list)  =
    let len = (List.head lines).Length
    let index = seq { for i in 0 .. len -> i}
    let t = countSingleBit lines
    index |> Seq.map t
    printfn "Power Consumption: %d" 0
    0
