// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1MergeV1 data =
    let rec helper acc data =
        match data with
        | [x] -> Analysis.mergePointClouds acc x
        | _ -> 
            let bestPartner, _ = Analysis.findBestMergePartner acc data
            helper (Analysis.mergePointClouds acc bestPartner) (data |> List.except [bestPartner])

    match data with
    | [x] -> x
    | x::xs -> helper x xs
    | _ -> failwith "ERROR, data contains no data set! :("

let part1MergeV2 data =
    let rec partnerSearch acc d =
        match d with
        | [] -> acc
        | [x] -> acc
        | x::xs -> (x, Analysis.findBestMergePartner x xs) :: acc

    let rec helper dlist =
        match dlist with
        | [x] -> x
        | _ -> 
            let allBestPartners = partnerSearch [] dlist
            let (pc1, (pc2, _)) = allBestPartners |> List.maxBy (fun (ael1, (ael2, mpl)) -> mpl |> List.length)
            
            let nextMerged = Analysis.mergePointClouds pc1 pc2
            helper (nextMerged::(dlist |> List.except [pc1; pc2]))

    match data with
    | [x] -> x
    | xs -> helper xs
    | _ -> failwith "ERROR, data contains no data set! :("

let part1ScansCountMerged data =
    let analyzedData = data |> List.map Analysis.analyze
    //let resPoints = analyzedData |> List.reduce Analysis.mergePointClouds
    let resPoints = part1MergeV2 analyzedData
    //printfn "  All points: %A" (resPoints |> List.map (fun ae -> ae.Point))
    let res = resPoints |> List.length
    printfn "Part 1, #points: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 19: Beacon Scanner\n======================\n"
    let data = ScannerReadings.loadData (DataInput.Puzzle)
    //printfn "Data: %A" data
    part1ScansCountMerged data
    0 // return an integer exit code
