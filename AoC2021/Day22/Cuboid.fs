
module Cuboid

type Range = { From: int; To: int }

//type Cuboid = {
//    X: Range
//    Y: Range
//    Z: Range
//    }

type Cuboid = Range array

type CuboidRelation = Unrelated | Equal | LcontainsR | RcontainsL | LoverlapsLeft | LoverlapsRight


//let prettify c = sprintf "(%d..%d/%d..%d/%d..%d)" c.X.From c.X.To c.Y.From c.Y.To c.Z.From c.Z.To
let prettify c = sprintf "(%s)" (String.concat "/" (c |> Array.map (fun r -> sprintf "%d..%d" r.From r.To)))

let calcVolume c: int64 =
    c |> Array.map (fun r -> (int64)r.To - (int64)r.From + 1L) |> Array.reduce (*)

let determineRelations cl cr =
    let compareRanges rl rr =
        match rl.From with
        | x when x>rr.To ->
            Unrelated
        | x when x > rr.From ->
            match rl.To with
            | y when y>=rr.To -> LoverlapsRight
            | _ -> RcontainsL
        | x when x = rr.From ->
            match rl.To with
            | y when y>rr.To -> LcontainsR
            | y when y=rr.To -> Equal
            | _ -> LoverlapsLeft
        | x -> // basically: x < rr.From
            match rl.To with
            | y when y<rr.From -> Unrelated
            | y when y<rr.To -> LoverlapsLeft
            | _ -> LcontainsR

    //[compareRanges cl.X cr.X; compareRanges cl.Y cr.Y; compareRanges cl.Z cr.Z]
    Array.map2 compareRanges cl cr

let areSomehowRelated r =
    r |> Array.forall (fun r -> r <> Unrelated)


let splitOnAxes cToSplit (cSplitOn: Cuboid) (relations: CuboidRelation array) =
    let split cToSplit (cSplitOn: Cuboid) axis: Cuboid list*Cuboid  =
        match relations.[axis] with
        | Unrelated -> failwith "Only related cuboid should be used in splitting"
        | Equal -> [], cToSplit
        | LcontainsR ->
            [ (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with To=cSplitOn.[axis].From - 1 }))
              (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with From=cSplitOn.[axis].To + 1 }))]
            , (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else cSplitOn.[axis]))
        | RcontainsL -> [], cToSplit
        | LoverlapsLeft ->
            [ (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with To=cSplitOn.[axis].From - 1 }))]
            , (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with From=cSplitOn.[axis].From }))
        | LoverlapsRight ->
            [ (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with From=cSplitOn.[axis].To + 1 }))]
            , (cToSplit |> Array.mapi (fun i r -> if (i<>axis) then r else { r with To=cSplitOn.[axis].To }))

    let unrelated1, toSplit1 = split cToSplit cSplitOn 0
    let unrelated2, toSplit2 = split toSplit1 cSplitOn 1
    let unrelated3, toSplit3 = split toSplit2 cSplitOn 2
    List.concat [unrelated1; unrelated2; unrelated3], toSplit3
