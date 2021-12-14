// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let expandOnce (inp: string) rules =
    let rec expandHelper inp (rules: Map<string, string>) acc =
        match inp with
        | x1::x2::xs ->
            let pair = System.String([|x1; x2|])
            if (rules.ContainsKey(pair)) then
                expandHelper (x2::xs) rules (rules.[pair].[0]::x1::acc)
            else
                expandHelper (x2::xs) rules (x1::acc)
        | x::[] -> List.rev (x :: acc)
        | [] -> List.rev acc

    (expandHelper (inp.ToCharArray() |> List.ofArray) rules []) |> List.toArray |> System.String

let part1CalcFrequencies (polymer: string) =
    let a = polymer.ToCharArray()
    let counts = Array.countBy id a
    printfn "  Counts: %A" counts
    counts |> Array.sortByDescending (fun (k, v) -> v)

let part1Expand10 template rules =
    let mutable polymer = template
    for _ in seq { for i in 1..10 -> i} do
        polymer <- expandOnce polymer rules
        //printfn "  Expanded: %s" polymer
    let freqs = part1CalcFrequencies polymer
    let (_, maxFreq), (_, minFreq) = freqs.[0], freqs.[freqs.Length - 1]
    printfn "  Frequencies: %d %d" maxFreq minFreq
    printfn "Part 1, most common - least common: %d" (maxFreq - minFreq)
    
let part2Expand40 template rules =
    let mutable polymer = template
    for _ in seq { for i in 1..40 -> i} do
        polymer <- expandOnce polymer rules
        //printfn "  Expanded: %s" polymer
    let freqs = part1CalcFrequencies polymer
    let (_, maxFreq), (_, minFreq) = freqs.[0], freqs.[freqs.Length - 1]
    printfn "  Frequencies: %d %d" maxFreq minFreq
    printfn "Part 2, most common - least common: %d" (maxFreq - minFreq)


[<EntryPoint>]
let main argv =
    printfn "Day 14: Extended Polymerization\n===============================\n"
    let template, rules = PolymerizationInput.loadData DataInput.Puzzle
    printfn "Template: %s" template
    printfn "Rules: %A" rules
    part1Expand10 template rules
    part2Expand40 template rules
    0 // return an integer exit code
