// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


type CorruptionCheckResult = IsCorrupt of char | NotCorrupt | Incomplete of (char list)

let checkLineForState (line: string) =
    let doSymbolsMatch a b =
        match (a, b) with
        | ('(', ')') -> true
        | ('[', ']') -> true
        | ('{', '}') -> true
        | ('<', '>') -> true
        | _ -> false
    let areSymbolsCorrupt a b =
        match (a, b) with
        | ('(', x) when x=']' || x='}' || x='>' -> true
        | ('[', x) when x=')' || x='}' || x='>' -> true
        | ('{', x) when x=')' || x=']' || x='>' -> true
        | ('<', x) when x=')' || x=']' || x='}' -> true
        | _ -> false
    let rec checkLineStateHelper stack inTape: CorruptionCheckResult =
        match inTape with
        | t::ts ->
            match stack with
            | x::xs when (doSymbolsMatch x t) -> checkLineStateHelper xs ts
            | x1::x2::xs when (doSymbolsMatch x2 x1) -> checkLineStateHelper xs inTape
            | x1::x2::_ when (areSymbolsCorrupt x2 x1) -> IsCorrupt x1
            | _ -> checkLineStateHelper (t::stack) ts
        | [] ->
            match stack with
            | [] -> NotCorrupt
            | xs  -> Incomplete xs
    let a = line.ToCharArray()
    checkLineStateHelper [] (a |> List.ofArray)

let part1CorruptedLinesSyntaxScore data =
    let calcVal c =
        match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0
    let isCorrupt r =
        match r with
        | IsCorrupt _ -> true
        | _ -> false
    let corruptSymbols = data |> Seq.map checkLineForState
                              |> Seq.filter isCorrupt
                              |> Seq.map (fun r ->
                                    match r with
                                    | IsCorrupt c -> c
                                    | _ -> ' ')
    let symValues = corruptSymbols |> Seq.map calcVal
    let res = symValues |> Seq.sum
    printfn "Part 1, syntax errors score: %d" res

let part2AutocompleteScore data =
    let isIncomplete r =
        match r with
        | Incomplete _ -> true
        | _ -> false
    let counterSymbol c =
        match c with
            | '(' -> ')'
            | '[' -> ']'
            | '{' -> '}'
            | '<' -> '>'
            | _ -> ' '
    let counterSymbols res =
        match res with
        | Incomplete syms -> syms |> Seq.map counterSymbol
        | _ -> seq []
    let symbolValue c =
        match c with
            | ')' -> 1L
            | ']' -> 2L
            | '}' -> 3L
            | '>' -> 4L
            | _ -> 0L
    let incompleteLines = data |> Seq.map checkLineForState
                               |> Seq.filter isIncomplete
    //printfn "  Incomplete lines: %A" (incompleteLines |> List.ofSeq)
    let completions = incompleteLines |> Seq.map counterSymbols
    let scores = completions |> Seq.map (fun c -> Seq.fold (fun (s: int64)  (c: char) -> 5L*s + (symbolValue c)) 0L c) |> List.ofSeq
    printfn "  Scores: %A" scores
    let res = (scores |> List.sort).[scores.Length / 2]
    printfn "Part 2, autocomplete score: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 10: Syntax Scoring\n======================\n"
    let data = DataInput.multipleRawLines DataInput.Puzzle
    part1CorruptedLinesSyntaxScore data
    part2AutocompleteScore data
    0 // return an integer exit code
