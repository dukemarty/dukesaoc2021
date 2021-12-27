
module AluProgram

open System
open AoC.Utils

type Register = W | X | Y | Z
type LVal = LREG of Register
type RVal = REG of Register | CONSTANT of int

type AluInstruction = INP of LVal | ADD of LVal*RVal | MUL of LVal*RVal | DIV of LVal*RVal | MOD of LVal*RVal | EQL of LVal*RVal

let private parseLval (s: string) =
    match s.ToUpper() with
    | "W" -> LREG W
    | "X" -> LREG X
    | "Y" -> LREG Y
    | "Z" -> LREG Z
    | _ -> failwithf "Invalid register provided: %s" s

let private parseRval (s: string) =
    match s.ToUpper() with
    | "W" -> REG W
    | "X" -> REG X
    | "Y" -> REG Y
    | "Z" -> REG Z
    | s ->
        try
            CONSTANT (s |> int)
        with _ ->
            failwithf "Invalid rval: %s" s

let private parseCommand cmd (parameters: string array) =
    let lval = parseLval parameters.[0]
    match cmd with
    | "inp" -> INP lval
    | _ ->
        let rval = parseRval parameters.[1]
        match cmd with
        | "add" -> ADD (lval, rval)
        | "mul" -> MUL (lval, rval)
        | "div" -> DIV (lval, rval)
        | "mod" -> MOD (lval, rval)
        | "eql" -> EQL (lval, rval)

let private parseLine (line: string) =
    let tokens = line.Split ' '
    parseCommand tokens.[0] tokens.[1..]


let loadProgram source =
    let raw = DataInput.multipleRawLines source
    raw |> Seq.map parseLine


