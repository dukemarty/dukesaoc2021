
open System


type FoldDirection =
    | X
    | Y


let xsteps x =
    -0.5 + sqrt(0.25 + 2.0*x)


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

let sn1 = SnailNumber (Num (Lit 1, Lit 2), Num (Num (Lit 3, Lit 4), Lit 5))
calcMagnitude sn1

type ParsePart = Left | Right

open System.Text.RegularExpressions
let rsn1 = "[[1,2],[[3,4],5]]"
let rsn2 = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"

let parseSnailNumber1 s =
    let rec helper (s:string) part =
        match s.[0] with
        | '[' ->
            let lval, (rem: string) = helper (s.[1..]) Left
            if (rem="") then
                match lval with
                | Num (l, r) -> lval, rem
                | _ -> failwithf "Could not proceed after parsing %O" lval
            else
                match rem.[0] with
                | ',' ->
                    let rval, rem = helper (rem.[1..]) Right
                    if (rem="") then
                        Num (lval, rval), rem
                    else
                        match rem.[0] with
                        | ']' -> Num (lval, rval), rem.[1..]
                        | _ -> Num (lval, rval), rem
                | _ -> failwithf "Encountered '%c' instead of ','" rem.[0]
        | c when (Char.IsDigit c) ->
            if (part = Left) then
                let m = Regex.Match(s, "(\d+),(.+)")
                let lval = Lit (Int32.Parse m.Groups.[1].Value)
                let rval, remainder = helper m.Groups.[2].Value Right
                Num (lval, rval), remainder
            else
                let m = Regex.Match(s, "(\d+)\](.+)")
                Lit (Int32.Parse m.Groups.[1].Value), m.Groups.[2].Value
        | c -> failwithf "Encountered %c" c

    let res, rem = helper s Left
    printf "Remainder after receiving result: %s" rem
    match res with
    | Num (l, r) -> SnailNumber (l, r)
    | _ -> failwith "Not a valid SnailNumber)"


let parseSnailNumber s =
    let rec helper (s:string)=
        match s.[0] with
        | '[' ->
            let lval, (rem: string) = helper (s.[1..])
            match rem.[0] with
            | ',' ->
                let rval, rem = helper (rem.[1..])
                match rem.[0] with
                | ']' -> Num (lval, rval), rem.[1..]
                | c -> failwithf "Encountered '%c' instead of ']'" c
            | _ -> failwithf "Encountered '%c' instead of ','" rem.[0]
        | c when (Char.IsDigit c) ->
            let m = Regex.Match(s, "(\d+)(.*)")
            Lit (Int32.Parse m.Groups.[1].Value), m.Groups.[2].Value
        | c -> failwithf "Encountered %c" c

    let res, rem = helper s
    printf "Remainder after receiving result: %s" rem
    match res with
    | Num (l, r) -> SnailNumber (l, r)
    | _ -> failwith "Not a valid SnailNumber)"


parseSnailNumber rsn1
parseSnailNumber rsn2

let explTest1Raw = "[[[[[9,8],1],2],3],4]"
let et1 = parseSnailNumber explTest1Raw

type ExplResult =
    | Nothing of SnailDigit
    | Propagate of int*int
    | PropLeft of int*SnailDigit
    | PropRight of SnailDigit*int

//let explode (l, r) =
//    Nothing (l, r)

let rec addLeft (sd: SnailDigit) v =
    match sd with
    | Lit n -> Lit (n+v)
    | Num (l, r) -> Num ((addLeft l v), r)

let rec addRight (sd: SnailDigit) v =
    match sd with
    | Lit n -> Lit (n+v)
    | Num (l, r) -> Num (l, (addLeft r v))


let rec tryExplode sn =
    let rec helper sd depth =
        if (depth = 4) then
            printfn "Reached depth 5 with: %O" sd
            match sd with
            | Num (Lit l, Lit r) -> Propagate (l, r)
            | _ -> failwith "Reached depth 5 but not with a literal!"
        else
            match sd with
            | Num (l, r) ->
                let resLeft = helper l (depth + 1)
                match resLeft with
                | Nothing _ ->
                    let resRight = helper r (depth + 1)
                    match resRight with
                    | Nothing _ -> Nothing sd
                    | Propagate (al, ar) -> PropRight ((addRight l al), ar)
                    | x -> failwithf "helper for right term returned %O" x
                | Propagate (al, ar) -> PropLeft (al, (addLeft r ar))
                | PropLeft (al, d) -> PropLeft (al, Num (d, r))
                | x -> failwithf "helper for left term returned %O" x
            | Lit _ -> Nothing sd

    let lval, rval = sn
    let resLeft = helper lval 1
    match resLeft with
    | Nothing s ->
        let resRight = helper rval 1
        match resRight with
        | Nothing t -> false, sn
        | Propagate (addL, addR) -> failwith "Received Propagate on mainlevel"
        | PropLeft (al, d) -> true, SnailNumber ((addLeft lval al), d)
        | PropRight (d, ar) -> true, SnailNumber (lval, d)
    | Propagate (addL, addR) -> failwith "Received Propagate on mainlevel"
    | PropLeft (al, d) -> true, SnailNumber (d, rval)
    | PropRight (d, ar) -> true, SnailNumber (d, (addRight rval ar))

tryExplode et1


let reduceSnailNumber sn =
    sn

let addSnailNumbers sn1 sn2 =
    let raw = (sn1, sn2)
    reduceSnailNumber raw


