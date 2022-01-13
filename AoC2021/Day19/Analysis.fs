
module Analysis

let distElements (a: Vector.Vec3d) (b: Vector.Vec3d) =
    //printfn "Dist of %A and %A" a b
    let dist = Vector.diff a b
    let compSet = dist |> Array.map abs |> Array.sort
    dist, compSet

type AnalysisElement =
    { Index: int
      Point: Vector.Vec3d
      Distances: (int array * int array) list
      Environment: Set<int array> }

let analyze d =
    let helper i p =
        let dists = d |> List.map (distElements p)
        let simplifiedSets = dists |> List.map snd |> Set.ofList

        { Index = i
          Point = p
          Distances = dists
          Environment = simplifiedSets }

    d |> List.mapi helper

type MatchingPair =
    { El1: AnalysisElement
      El2: AnalysisElement
      Congruences: Set<int array> }

let findPossibleMatches d1 d2 =
    let helper (d: AnalysisElement) =
        let x =
            d2
            |> List.mapi (fun i (e: AnalysisElement) -> e, Set.intersect d.Environment e.Environment)
            |> List.maxBy (fun (_, s) -> s.Count)

        let e, s = x

        if (s.Count > 1) then
            Some { El1 = d; El2 = e; Congruences = s }
        else
            None


    d1
    |> List.map helper
    |> List.filter (fun (t: MatchingPair option) -> t.IsSome)
    |> List.map
        (fun mpo ->
            match mpo with
            | Some mp -> mp
            | None -> failwith "Huiuiui, eigentlich sollten None's ausgefiltert sein")


let determineRotation matchingPairs =
    let allRelCongr =
        matchingPairs
        |> List.map (fun tm -> tm, tm.Congruences |> Set.filter (fun a -> (Set.ofArray a).Count = 3))
        |> List.map (fun (tm, rc) -> rc |> Set.map (fun a -> fst (tm.El1.Distances |> List.find (fun t -> snd t = a)), fst (tm.El2.Distances |> List.find (fun t -> snd t = a))))
    let allTMs = allRelCongr |> List.map (fun tl -> tl |> Seq.map Vector.determineTransformationMatrix |> Seq.countBy id)
    //printfn "  #rotations: %A" (allTMs |> List.map (fun l -> l |> Seq.length))
    let bestFitTMs = allTMs |> List.map (fun s -> fst(s |> Seq.maxBy snd))
    let singleBestFitTM = fst (bestFitTMs |> List.countBy id |> List.maxBy snd)
    printfn "  Determined %A" singleBestFitTM

    //let dbg = allRelCongr |> List.map (fun s -> s |> Set.map (fun (a, b) -> (a, b, Vector.applyRotation singleBestFitTM b)))
    //printfn "  For verification: matched and rotated distances:\n%A" dbg

    singleBestFitTM

let determineFullTransformation d1 d2 =
    let matchingPairs = findPossibleMatches d1 d2
    printfn "  #matching pairs: %d" (matchingPairs |> List.length)
    let rotation = determineRotation matchingPairs

    let pointPairs = matchingPairs |> List.map (fun mp -> mp.El1.Point, mp.El2.Point)
    let rotatedPairs = pointPairs |> List.map (fun (p1, p2) -> p1, (Vector.applyRotation rotation p2))
    
    let translations = rotatedPairs 
                       |> List.map (fun (p1, p2) -> fst (distElements p1 p2))
                       |> List.countBy id
    let translation: Vector.Vec3d = fst (translations |> List.maxBy snd)
    //let translation: Vector.Vec3d = fst (rotatedPairs 
    //                  |> List.map (fun (p1, p2) -> fst (distElements p1 p2))
    //                  |> List.countBy id
    //                  |> List.maxBy snd)
    rotation, translation

let mergePointClouds d1 d2 =
    let rot, transl = determineFullTransformation d1 d2
    let pl1 = d1 |> List.map (fun ae -> ae.Point)
    let pl2 = d2 |> List.map (fun ae -> Vector.transform rot transl ae.Point)
    let mergedPoints = Set.union (pl1 |> Set.ofList) (pl2 |> Set.ofList)
    analyze (mergedPoints |> Set.toList), { Vector.FullTransform.Rotation=rot; Vector.FullTransform.Translation=transl }
    
let findBestMergePartner d1 dlist =
    dlist
    |> List.map (fun d -> d, (findPossibleMatches d1 d))
    |> List.maxBy (fun (d, ml) -> ml |> List.length)