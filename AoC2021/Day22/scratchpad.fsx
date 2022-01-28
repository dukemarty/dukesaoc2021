open System
open System.Text.RegularExpressions

type RebootAction =
    | On
    | Off

type Range = { From: int; To: int }

type RebootStep =
    { Action: RebootAction
      X: Range
      Y: Range
      Z: Range }

let parseRebootStep line =
    let m =
        Regex.Match(line, "(\w+)\s+x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)")

    let action =
        match m.Groups.[1].Value with
        | "on" -> On
        | "off" -> Off
        | s -> failwithf "Unknown action found: %s" s

    { Action = action
      X =
          { From = (Int32.Parse m.Groups.[2].Value)
            To = (Int32.Parse m.Groups.[3].Value) }
      Y =
          { From = (Int32.Parse m.Groups.[4].Value)
            To = (Int32.Parse m.Groups.[5].Value) }
      Z =
          { From = (Int32.Parse m.Groups.[6].Value)
            To = (Int32.Parse m.Groups.[7].Value) } }

type Cuboid = Range array

let getCuboid (rs: RebootStep) = [| rs.X; rs.Y; rs.Z |]

let sl = ["on x=10..12,y=10..12,z=10..12"; "on x=11..13,y=11..13,z=11..13"; "off x=9..11,y=9..11,z=9..11"; "on x=10..10,y=10..10,z=10..10" ]
let data = sl |> List.map (parseRebootStep >> getCuboid)

let prettify c = sprintf "(%s)" (String.concat "/" (c |> Array.map (fun r -> sprintf "%d..%d" r.From r.To)))

data |> List.iter (fun c -> printfn "%s" (prettify c))

type CuboidRelation = Unrelated | Equal | LcontainsR | RcontainsL | LoverlapsLeft | LoverlapsRight

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

    Array.map2 compareRanges cl cr

let r1 = determineRelations data.[0] data.[1]
let r2 = determineRelations data.[0] data.[2]
let r3 = determineRelations data.[0] data.[3]
let r4 = determineRelations data.[1] data.[2]
let r5 = determineRelations data.[1] data.[3]
let r6 = determineRelations data.[2] data.[3]

let areSomehowRelated r =
    r |> Array.forall (fun r -> r <> Unrelated)

areSomehowRelated r1
areSomehowRelated r2
areSomehowRelated r3
areSomehowRelated r4
areSomehowRelated r5
areSomehowRelated r6

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

splitOnAxes data.[0] data.[1] r1

//let splitByIntersection cubToSplit cubTrigger =
//    let rel = determineRelations cubToSplit cubTrigger
//    splitOnAxes ([], [cubToSplit], cubTrigger) rel


