// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1MergeV2 data =
    let rec partnerSearch acc d =
        match d with
        | [] -> acc
        | [x] -> acc
        | x::xs -> (x, Analysis.findBestMergePartner x xs) :: acc

    let rec helper dlist trafos =
        match dlist with
        | [x] -> x, trafos
        | _ -> 
            let allBestPartners = partnerSearch [] dlist
            let (pc1, (pc2, _)) = allBestPartners |> List.maxBy (fun (ael1, (ael2, mpl)) -> mpl |> List.length)
            
            let nextMerged, nextTrafo = Analysis.mergePointClouds pc1 pc2
            helper (nextMerged::(dlist |> List.except [pc1; pc2])) (nextTrafo :: trafos)

    match data with
    | [x] -> x, []
    | xs -> helper xs []
    | _ -> failwith "ERROR, data contains no data set! :("


let part1ScansCountMerged data =
    let analyzedData = data |> List.map Analysis.analyze
    //let resPoints = analyzedData |> List.reduce Analysis.mergePointClouds
    let resPoints, trafos = part1MergeV2 analyzedData
    //printfn "  All points: %A" (resPoints |> List.map (fun ae -> ae.Point))
    let res = resPoints |> List.length
    printfn "Part 1, #points: %d" res
    resPoints, trafos


let part2MostApartScanners (trafos: Vector.FullTransform list) =
    let calcAllDists t =
        t 
        |> List.mapi (fun i x -> t.[i..]
                                 |> List.map (fun u -> Vector.diff x u)
                                 |> List.map Vector.manhattanLength)
        |> List.collect id

    let allTranslations = [|0;0;0|]::(trafos |> List.map (fun t -> t.Translation))
    printfn "  All translations: %A" allTranslations
    let res = allTranslations |> calcAllDists |> List.max
    printfn "Part 2, largest distance: %d" res


[<EntryPoint>]
let main argv =
    printfn "Day 19: Beacon Scanner\n======================\n"
    let data = ScannerReadings.loadData (DataInput.Puzzle)
    //printfn "Data: %A" data
    let _, trafos = part1ScansCountMerged data
    part2MostApartScanners trafos
    0 // return an integer exit code
