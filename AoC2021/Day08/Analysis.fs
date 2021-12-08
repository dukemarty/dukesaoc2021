module Analysis

type SignalMapping = { Orig: char; Image: char }

type DigitMapping = { Orig: int; Image: string }

type DigitHypothesis =
    { Orig: int
      Candidates: (char array) list }

type SignalHypothesis =
    { Orig: char
      Candidates: (char array) list }


let createDigitHypothesis orig cand = { DigitHypothesis.Orig = orig; Candidates = cand }

let createSignalMapping orig image = { SignalMapping.Orig = orig; Image = image }

let createDigitMapping orig image = { DigitMapping.Orig = orig; Image = image }

let getDigitElements (mapping: DigitMapping seq) digit =
    (Seq.find (fun (m: DigitMapping) -> m.Orig = digit) mapping).Image.ToCharArray() |> Set.ofArray


let determineSingleDifference (digMapping: DigitMapping seq) a b =
    let aParts = getDigitElements digMapping a
    let bParts = getDigitElements digMapping b
    (Set.difference aParts bParts |> List.ofSeq).Head


let findMappingA digiMapping1478 =
    createSignalMapping 'a'  (determineSingleDifference digiMapping1478 7 1)

let private covers (l: Set<char>) (r: Set<char>) =
    (Set.difference r l).IsEmpty

let private equals (l: char array) (r: Set<char>) =
    let lStr = l |> Array.sort |> System.String
    let rStr = r |> Set.toArray |> Array.sort |> System.String
    lStr = rStr

let findRemainingMappings (hyp: DigitHypothesis list) digiMapping1478 =
    let zero = hyp.[0].Candidates |> List.find (fun c -> (covers (set c) (getDigitElements digiMapping1478 1)) && not (covers (set c) (getDigitElements digiMapping1478 4)))
    let nine = hyp.[9].Candidates |> List.find (fun c -> covers (set c) (getDigitElements digiMapping1478 4))
    let digiMapping014789 = Seq.append digiMapping1478 (seq { (createDigitMapping 0 (zero |> Array.sort |> System.String)); (createDigitMapping 9 (nine |> Array.sort |> System.String)) })
    let six = hyp.[6].Candidates |> List.find (fun c -> (not (equals c (getDigitElements digiMapping014789 0)) && (not (equals c (getDigitElements digiMapping014789 9)))))
    let digiMapping0146789 = Seq.append digiMapping014789 (seq { (createDigitMapping 6 (six |> Array.sort |> System.String)) })
    let two = hyp.[2].Candidates |> List.find (fun c -> not (covers (getDigitElements digiMapping0146789 9) (set c)))
    let three = hyp.[3].Candidates |> List.find (fun c -> (not (covers (getDigitElements digiMapping0146789 6) (set c))) && (not (equals c (set two))))
    let digiMapping012346789 = Seq.append digiMapping0146789 (seq { (createDigitMapping 2 (two |> Array.sort |> System.String)); (createDigitMapping 3 (three |> Array.sort |> System.String)) })
    let five = hyp.[5].Candidates |> List.find (fun c -> (not (equals c (getDigitElements digiMapping012346789 2)) && (not (equals c (getDigitElements digiMapping012346789 3)))))
    let digiMappingFull = Seq.append digiMapping012346789 (seq { (createDigitMapping 5 (five |> Array.sort |> System.String)) })
    digiMappingFull

let identifyMissingMappings (startHyp: DigitHypothesis list) (baseDigMapping: DigitMapping seq) =
    let res= findRemainingMappings startHyp baseDigMapping
    //printfn "Mappings: %A" (res |> List.ofSeq)
    res

let decode (mapping: DigitMapping seq) (code: string) =
    (mapping |> Seq.find (fun m -> m.Image = code)).Orig
