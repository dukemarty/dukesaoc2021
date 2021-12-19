module SnailNumber

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
    | Num (l, r) -> Num (l, (addLeft r v))


let rec private tryExplode sn =
    //let rec tryExplode s depth =
    //    match s with
    //    | Num (l, r) ->
    //        if (depth > 4) then
    //            expl
    //    | Lit _ -> false, s
    //    false, s
    false, sn

let rec reduceSnailNumber sn =
    let trySplit s =
        false, s
    let b, res = tryExplode sn
    if (b) then
        reduceSnailNumber res
    else
        let b, res = trySplit sn
        if (b) then
            reduceSnailNumber res
        else
            sn

let addSnailNumbers sn1 sn2 =
    let raw = (sn1, sn2)
    reduceSnailNumber raw