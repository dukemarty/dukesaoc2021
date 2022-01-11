open System

let isEmptyLine s = String.IsNullOrWhiteSpace s


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

type Vec3d = int array

let parse3dPos (s: string) : Vec3d =
    s.Split(',') |> Seq.map Int32.Parse |> Array.ofSeq

let s =
    "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390"


let parseScannerBlock block = List.tail block |> List.map parse3dPos

let data =
    let blocks =
        s.Split '\n'
        |> List.ofArray
        |> splitListOnElem isEmptyLine

    blocks |> List.map parseScannerBlock

let vecDiff (a: Vec3d) (b: Vec3d) = Array.map2 (-) a b
let vecSum (a: Vec3d) (b: Vec3d) = Array.map2 (+) a b

vecDiff [| 1; 2; 3 |] [| 6; 5; 4 |]

let movePoint (a: Vec3d) (b: Vec3d) = vecSum a b
    

let distElements (a: Vec3d) (b: Vec3d) =
    //printfn "Dist of %A and %A" a b
    let dist = vecDiff a b
    let compSet = dist |> Array.map abs |> Array.sort
    dist, compSet

distElements [| 1; 2; 3 |] [| 6; 5; 4 |]

type AnalysisElement =
    { Index: int
      Point: Vec3d
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

let analyzedData = data |> List.map analyze

type MatchingPair =
    { El1: AnalysisElement
      El2: AnalysisElement
      Congruences: Set<int array> }

let findPossibleMatches d1 d2 =
    let helper (d: AnalysisElement) =
        let x =
            d2
            |> List.mapi (fun i (e: AnalysisElement) -> i, e, Set.intersect d.Environment e.Environment)
            |> List.maxBy (fun (_, _, s) -> s.Count)

        let i, e, s = x

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

let findBestMatch c d =
    c
    |> List.map
        (fun (p, s) ->
            p,
            s,
            (d
             |> List.mapi (fun j (q, t) -> (j, q, t, Set.intersect s t))))
    |> List.map (fun (p, s, l) -> p, s, l |> List.maxBy (fun (j, q, t, sit) -> sit.Count))
    |> List.filter (fun (p, s, (j, q, t, sit)) -> sit.Count > 1)

//let match1 = findBestMatch analyzedData.[0] analyzedData.[1]

let match2 =
    findPossibleMatches analyzedData.[0] analyzedData.[1]


let tm = match2.[0]
let relevantCongruences = tm.Congruences |> Set.filter (fun a -> (Set.ofArray a).Count = 3)
let rc = relevantCongruences |> Set.map (fun a -> fst (tm.El1.Distances |> List.find (fun t -> snd t = a)), fst (tm.El2.Distances |> List.find (fun t -> snd t = a)))

let allRelCongr =
    match2
    |> List.map (fun tm -> tm, tm.Congruences |> Set.filter (fun a -> (Set.ofArray a).Count = 3))
    |> List.map (fun (tm, rc) -> rc |> Set.map (fun a -> fst (tm.El1.Distances |> List.find (fun t -> snd t = a)), fst (tm.El2.Distances |> List.find (fun t -> snd t = a))))


let determineTransformationMatrix (t: (int array)*(int array)) =
    let a, b = t
    [0..2] |> List.map (fun i -> a |> Array.map (fun aj -> if (abs(aj)<>abs(b.[i])) then 0 else (if (aj*b.[i] > 0) then 1 else -1)))

let allTMs = allRelCongr |> List.map (fun tl -> tl |> Seq.map determineTransformationMatrix |> Seq.countBy id)
let bestFitTMs = allTMs |> List.map (fun s -> fst(s |> Seq.maxBy snd))
let singleBestFitTM = fst (bestFitTMs |> List.countBy id |> List.maxBy snd)

let t = (Set.toList rc).[0]

determineTransformationMatrix t

rc |> Seq.map determineTransformationMatrix |> Seq.countBy id


let applyTrafo (t: int array list) (p: Vec3d): Vec3d =
    t |> List.map (fun r -> (Array.map2 (*) r p) |> Array.sum) |> Array.ofList

singleBestFitTM
let pointPairs = match2 |> List.map (fun mp -> mp.El1.Point, mp.El2.Point)
let rotPairs = pointPairs |> List.map (fun (p1, p2) -> p1, (applyTrafo singleBestFitTM p2))

let transl = fst (rotPairs |> List.map (fun (p1, p2) -> fst (distElements p1 p2)) |> List.countBy id |> List.maxBy snd)

let transformCoordinate rot transl p =
    movePoint (applyTrafo rot p) transl

pointPairs |> List.map (fun (p1, p2) -> p1, (transformCoordinate singleBestFitTM transl p2))

