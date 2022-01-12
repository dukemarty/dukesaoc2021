module Vector

type Vec3d = int array
type Trafo = int array array

let diff (a: Vec3d) (b: Vec3d) = Array.map2 (-) a b
let sum (a: Vec3d) (b: Vec3d) = Array.map2 (+) a b

let determineTransformationMatrix (t: Vec3d * Vec3d): Trafo =
    let a, b = t

    [ 0 .. 2 ]
    |> List.map
        (fun i ->
            b
            |> Array.map
                (fun bj ->
                    if (abs (bj) <> abs (a.[i])) then
                        0
                    else
                        (if (bj * a.[i] > 0) then 1 else -1)))
    |> Array.ofList

let applyRotation (t: Trafo) (p: Vec3d): Vec3d =
    t |> Array.map (fun r -> (Array.map2 (*) r p) |> Array.sum)

let applyTranslation (t: Vec3d) (p: Vec3d): Vec3d =
    sum p t

let transform rot transl p =
    applyTranslation transl (applyRotation rot p) 
