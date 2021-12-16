// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let rec sumVersions packets = 
    let sumHelper packet =
        match packet with
        | Bitstream.Lit (v, _) -> v
        | Bitstream.Op (v, _, ps) -> v + (sumVersions ps)

    match packets with
    | [p] -> sumHelper p
    | p::ps -> (sumHelper p) + (sumVersions ps)

let part1VersionsSum bits =
    let packets, consumedCount = Bitstream.parsePackets bits
    printfn "  Packets : %A" packets
    printfn "  Consumed: %d" consumedCount
    let res = sumVersions packets
    printfn "Part 1, sum of all versions: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 16: Packet Decoder\n======================\n"

    let bitstream: Bitstream.Bitstream =
        (DataInput.singleRawLine (DataInput.Puzzle))
            .ToCharArray()
        |> Seq.map (fun c -> Convert.ToInt32(string c, 16))
        |> Seq.map (fun n -> Convert.ToString(n, 2).PadLeft(4, '0'))
        |> String.concat ""

    printfn "Data: %s" bitstream
    part1VersionsSum bitstream
    0 // return an integer exit code
