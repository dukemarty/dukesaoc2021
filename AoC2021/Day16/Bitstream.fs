module Bitstream

open System

type Bitstream = string

type ParseMode =
    | SumLength of int
    | PackagesCount of int

let splitHeader (bits: char array) =
    let version =
        Convert.ToInt32(bits.[0..2] |> System.String, 2)

    let typeId =
        Convert.ToInt32(bits.[3..5] |> System.String, 2)

    let data = bits.[6..]
    version, typeId, data

let parse4Literal bits =
    let rec literalParseHelper (bits: char array) =
        match bits.[0] with
        | '0' -> (bits.[1..4] |> System.String), 5
        | '1' ->
            let remainder, count = literalParseHelper bits.[5..]
            (bits.[1..4] |> System.String) + remainder, count + 5

    let extractedBits, consumedCount = literalParseHelper bits
    Convert.ToUInt64(extractedBits, 2), consumedCount

let parseOperationHeader (bits: char array) =
    match bits.[0] with
    | '0' ->
        let totalSubpacketLength =
            Convert.ToInt32(bits.[1..15] |> System.String, 2)

        SumLength totalSubpacketLength, 16, bits.[16..]
    | '1' ->
        let numberOfSubpackets =
            Convert.ToInt32(bits.[1..11] |> System.String, 2)

        PackagesCount numberOfSubpackets, 12, bits.[12..]
    | c -> failwithf "Unexpected (impossible) character: %c" c

let parsePackets (bitstream: string) =

    let rec parseNextPacket bits mode =
        let v, t, d = splitHeader bits
        let headerConsumedCount = 6

        let p, consumedCount =
            match t with
            | 4 ->
                let literalValue, consumed = parse4Literal d
                printfn "  Decoded literal: %d" literalValue
                ExprTree.Lit(v, literalValue), headerConsumedCount + consumed
            | _ ->
                let remMode, opHeaderConsumedCount, remBits = parseOperationHeader d
                match remMode with
                | SumLength l ->
                    let ps, consumed = parseNextPacket remBits.[0..(l-1)] remMode
                    ExprTree.Op (v, t, ps), headerConsumedCount + opHeaderConsumedCount + consumed
                | PackagesCount n ->
                    let ps, consumed = parseNextPacket remBits remMode
                    ExprTree.Op (v, t, ps), headerConsumedCount + opHeaderConsumedCount + consumed

        match mode with
        | SumLength n when (n - consumedCount) < 7 -> [p], consumedCount
        | SumLength n ->
            let nps, addConsumedCount =
                parseNextPacket bits.[consumedCount..] (SumLength(n - consumedCount))

            p :: nps, consumedCount + addConsumedCount
        | PackagesCount 1 -> [p], consumedCount
        | PackagesCount n ->
            let nps, addConsumedCount =
                parseNextPacket bits.[consumedCount..] (PackagesCount(n - 1))

            p :: nps, consumedCount + addConsumedCount

    let bits = bitstream.ToCharArray()
    parseNextPacket bits (PackagesCount 1)
