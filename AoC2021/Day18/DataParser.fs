
module DataParser

open System
open System.Text.RegularExpressions
open AoC.Utils

type private ParsePart = Left | Right

let parseSnailNumber1 s =
    let rec helper (s:string) part =
        match s.[0] with
        | '[' ->
            let lval, (rem: string) = helper (s.[1..]) Left
            if (rem="") then
                match lval with
                | SnailNumber.Num (l, r) -> lval, rem
                | _ -> failwithf "Could not proceed after parsing %O" lval
            else
                match rem.[0] with
                | ',' ->
                    let rval, rem = helper (rem.[1..]) Right
                    if (rem="") then
                        SnailNumber.Num (lval, rval), rem
                    else
                        match rem.[0] with
                        | ']' -> SnailNumber.Num (lval, rval), rem.[1..]
                        | _ -> SnailNumber.Num (lval, rval), rem
                | _ -> failwithf "Encountered '%c' instead of ','" rem.[0]
        | c when (Char.IsDigit c) ->
            if (part = Left) then
                let m = Regex.Match(s, "(\d+),(.+)")
                let lval = SnailNumber.Lit (Int32.Parse m.Groups.[1].Value)
                let rval, remainder = helper m.Groups.[2].Value Right
                SnailNumber.Num (lval, rval), remainder
            else
                let m = Regex.Match(s, "(\d+)\](.+)")
                SnailNumber.Lit (Int32.Parse m.Groups.[1].Value), m.Groups.[2].Value
        | c -> failwithf "Encountered %c" c

    let res, rem = helper s Left
    printf "Remainder after receiving result: %s" rem
    match res with
    | SnailNumber.Num (l, r) -> SnailNumber.SnailNumber (l, r)
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
                | ']' -> SnailNumber.Num (lval, rval), rem.[1..]
                | c -> failwithf "Encountered '%c' instead of ']'" c
            | _ -> failwithf "Encountered '%c' instead of ','" rem.[0]
        | c when (Char.IsDigit c) ->
            let m = Regex.Match(s, "(\d+)(.*)")
            SnailNumber.Lit (Int32.Parse m.Groups.[1].Value), m.Groups.[2].Value
        | c -> failwithf "Encountered %c" c

    let res, rem = helper s
    match res with
    | SnailNumber.Num (l, r) -> SnailNumber.SnailNumber (l, r)
    | _ -> failwith "Not a valid SnailNumber)"

let loadData source =
    let rawData = DataInput.multipleRawLines source
    rawData |> List.map parseSnailNumber
