
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

let explTestRaw = [
    "[[[[[9,8],1],2],3],4]"
    "[7,[6,[5,[4,[3,2]]]]]"
    "[[6,[5,[4,[3,2]]]],1]"
    "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
    "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
    ]

let ets = explTestRaw |> Seq.map parseSnailNumber

type ExplResult =
    | Nothing of SnailDigit
    | Propagate of SnailDigit*int*int
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
        if (depth = 5) then
            printfn "Reached depth 5 with: %O" sd
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
                    | PropRight (d, ar) -> PropRight (Num (l, d), ar)
                    | x -> failwithf "helper for right term returned %O" x
                | Propagate (d, al, ar) -> PropLeft (al, Num (d, (addLeft r ar)))
                | PropLeft (al, d) -> PropLeft (al, Num (d, r))
                | x -> failwithf "helper for left term returned %O" x
            | Lit _ -> Nothing sd

    let lval, rval = sn
    let resLeft = helper lval 2
    match resLeft with
    | Nothing s ->
        let resRight = helper rval 2
        match resRight with
        | Nothing t -> false, sn
        | Propagate (d,addL, addR) -> failwith "Received Propagate on mainlevel"
        | PropLeft (al, d) -> true, SnailNumber ((addLeft lval al), d)
        | PropRight (d, ar) -> true, SnailNumber (lval, d)
    | Propagate (d, addL, addR) -> failwith "Received Propagate on mainlevel"
    | PropLeft (al, d) -> true, SnailNumber (d, rval)
    | PropRight (d, ar) -> true, SnailNumber (d, (addRight rval ar))

ets |> Seq.map tryExplode |> List.ofSeq


let splitTestRaw = [
    "[11,4]"
    "[7,11]"
    "[[6,[5,[4,[11,2]]]],1]"
    "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,11]]]]]"
    "[[3,[2,[1,[7,11]]]],[6,[5,[4,[3,2]]]]]"
    ]

let sts = splitTestRaw |> Seq.map parseSnailNumber |> List.ofSeq

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
    | _ -> failwith "Result of trySplit-helperis not a Num!"


sts |> Seq.map trySplit |> List.ofSeq

let reduceSnailNumber sn =
    sn

let addSnailNumbers sn1 sn2 =
    let raw = (sn1, sn2)
    reduceSnailNumber raw


