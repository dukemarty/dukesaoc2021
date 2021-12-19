module SnailNumber

open System

type SnailDigit =
    | Lit of int
    | Num of SnailDigit * SnailDigit

type SnailNumber = SnailDigit * SnailDigit


let rec calcMagnitude (sn: SnailNumber) =
    let helper sd =
        match sd with
        | Lit n -> n
        | Num (l, r) -> calcMagnitude (l, r)

    let (l, r) = sn
    3 * (helper l) + 2 * (helper r)

let rec addLeft (sd: SnailDigit) v =
    match sd with
    | Lit n -> Lit (n+v)
    | Num (l, r) -> Num ((addLeft l v), r)

let rec addRight (sd: SnailDigit) v =
    match sd with
    | Lit n -> Lit (n+v)
    | Num (l, r) -> Num (l, (addRight r v))


type private ExplResult =
    | Nothing of SnailDigit
    | Propagate of SnailDigit*int*int
    | PropLeft of int*SnailDigit
    | PropRight of SnailDigit*int
    | Exploded of SnailDigit

let rec tryExplode sn =
    let rec helper sd depth =
        if (depth = 5) then
            //printfn "  Reached depth 5 with: %O" sd
            match sd with
            | Num (Lit l, Lit r) -> Propagate (Lit 0, l, r)
            | Lit a -> Nothing sd
        else
            match sd with
            | Num (l, r) ->
                let resLeft = helper l (depth + 1)
                match resLeft with
                | Nothing _ ->
                    let resRight = helper r (depth + 1)
                    match resRight with
                    | Nothing _ -> Nothing sd
                    | Propagate (d, al, ar) -> PropRight (Num ((addRight l al),d), ar)
                    | PropLeft (al, d) -> Exploded (Num ((addRight l al), d))
                    | PropRight (d, ar) -> PropRight (Num (l, d), ar)
                    | Exploded d -> Exploded (Num (l, d))
                    | x -> failwithf "helper for right term returned %O" x
                | Propagate (d, al, ar) -> PropLeft (al, Num (d, (addLeft r ar)))
                | PropLeft (al, d) -> PropLeft (al, Num (d, r))
                | PropRight (d, ar) -> Exploded (Num (d, (addLeft r ar)))
                | Exploded d -> Exploded (Num (d, r))
                | x -> failwithf "helper for left term returned %O" x
            | Lit _ -> Nothing sd

    let lval, rval = sn
    let resLeft = helper lval 2
    match resLeft with
    | Nothing s ->
        let resRight = helper rval 2
        match resRight with
        | Nothing t -> false, sn
        | Propagate (d, addL, addR) -> failwith "Received Propagate on mainlevel"
        | PropLeft (al, d) -> true, SnailNumber ((addRight lval al), d)
        | PropRight (d, ar) -> true, SnailNumber (lval, d)
        | Exploded d -> true, SnailNumber (lval, d)
    | Propagate (d, addL, addR) -> failwith "Received Propagate on mainlevel"
    | PropLeft (al, d) -> true, SnailNumber (d, rval)
    | PropRight (d, ar) -> true, SnailNumber (d, (addLeft rval ar))
    | Exploded d -> true, SnailNumber (d, rval)

let trySplit sn =
    let rec helper sd =
        match sd with
        | Lit n when (n >= 10) -> true, Num (Lit (int(Math.Floor(float n / 2.0))), Lit (int(Math.Ceiling(float n / 2.0))))
        | Lit _ -> false, sd
        | Num (l, r) ->
            let didSplit, d = helper l
            if (didSplit) then
                true, Num (d, r)
            else
                let didSplit, d = helper r
                if (didSplit) then
                    true, Num (l, d)
                else
                    false, sd
    let sd = Num sn
    let res, resSd = helper sd
    match resSd with
    | Num (l, r) -> res, (l, r)
    | _ -> failwith "Result of trySplit-helper is not a Num!"

let rec reduceSnailNumber sn =
    let b, res = tryExplode sn
    if (b) then
        //printfn "  Reduced                 : %O" sn
        //printfn "  Reduced via explosion to: %O" res
        reduceSnailNumber res
    else
        let b, res = trySplit sn
        if (b) then
            //printfn "  Reduced             : %O" sn
            //printfn "  Reduced via split to: %O" res
            reduceSnailNumber res
        else
            sn

let addSnailNumbers sn1 sn2 =
    //printfn "    Added %O + %O" sn1 sn2
    let raw = (Num sn1, Num sn2)
    let res = reduceSnailNumber raw
    //printfn "  summed to %O" res
    res