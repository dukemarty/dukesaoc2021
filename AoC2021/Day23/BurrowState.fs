
module BurrowState

open AoC.Utils

(*
Form of the burrow:

(00)(01)(02)(05)(06)(09)(10)(13)(14)(17)(18)
        (03)    (07)    (11)    (15)
        (04)    (08)    (12)    (16)
*)

// used for distance calculations
let locationPositions =
    [|
        (0, 0) // 0
        (1, 0)
        (2, 0) // 2
        (2, 1)
        (2, 2) // 4
        (3, 0)
        (4, 0) // 6
        (4, 1)
        (4, 2) // 8
        (5, 0)
        (6, 0) // 10
        (6, 1)
        (6, 2) // 12
        (7, 0)
        (8, 0) // 14
        (8, 1)
        (8, 2) // 16
        (9, 0)
        (10, 0) // 18
    |]

let reachableFromLocation =
    [|
        [ 1 ] // 0
        [ 0; 2 ]
        [ 1; 3; 5 ] // 2
        [ 2; 4 ]
        [ 3 ] // 4
        [ 2; 6 ]
        [ 5; 7; 9 ] // 6
        [ 6; 8 ]
        [ 7 ] // 8
        [ 6; 10 ]
        [ 9; 11; 13 ] // 10
        [ 10; 12 ]
        [ 11; ] // 12
        [ 10; 14]
        [ 13; 15; 17 ] // 14
        [ 14; 16 ]
        [ 15 ] // 16
        [ 14; 18 ]
        [ 17 ] // 18
    |]

let notWaitLocations = [ 2; 6; 10; 14 ] |> Set.ofList

let private corridorOrder = [ 0; 1; 2; 5; 6; 9; 10; 13; 14; 17; 18 ]
let corridorLocations = corridorOrder |> Set.ofList

let targetsForAmphipods =
    Map.empty.
        Add('A', [ 3; 4 ] |> Set.ofList).
        Add('B', [ 7; 8 ] |> Set.ofList).
        Add('C', [ 11; 12 ] |> Set.ofList).
        Add('D', [ 15; 16 ] |> Set.ofList)

type Amphipod = {
    Type: char
    Location: int }

let loadInitialState source =
    let raw = DataInput.multipleRawLines source
    [
        { Type=raw.[2].[3]; Location=3 }
        { Type=raw.[2].[5]; Location=4 }
        { Type=raw.[2].[7]; Location=7 }
        { Type=raw.[2].[9]; Location=8 }
        { Type=raw.[3].[3]; Location=11 }
        { Type=raw.[3].[5]; Location=12 }
        { Type=raw.[3].[7]; Location=15 }
        { Type=raw.[3].[9]; Location=16 }
    ]

let positionsBetween s e =
    if (s < e) then
        corridorOrder.[((corridorOrder |> List.findIndex (fun n -> n = s))+1)..(corridorOrder |> List.findIndex (fun n -> n = (e - 1)))]
    else
        corridorOrder.[(corridorOrder |> List.findIndex (fun n -> n = (e - 1)))..((corridorOrder |> List.findIndex (fun n -> n = s))-1)]
