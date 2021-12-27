

module AluSimulator


type AluState = {
    W: int
    X: int
    Y: int
    Z: int
}

let private changeReg state (reg: AluProgram.LVal) value =
    match reg with
    | AluProgram.LREG AluProgram.W -> { state with W=value }
    | AluProgram.LREG AluProgram.X -> { state with X=value }
    | AluProgram.LREG AluProgram.Y -> { state with Y=value }
    | AluProgram.LREG AluProgram.Z -> { state with Z=value }

let private getReg state (reg: AluProgram.Register) =
    match reg with
    | AluProgram.W -> state.W
    | AluProgram.X -> state.X
    | AluProgram.Y -> state.Y
    | AluProgram.Z -> state.Z

let private evaluateL state (ref: AluProgram.LVal) =
    match ref with
    | AluProgram.LREG r -> getReg state r

let private evaluateR state (ref: AluProgram.RVal) =
    match ref with
    | AluProgram.CONSTANT v -> v
    | AluProgram.REG r -> getReg state r

let private execute (command: AluProgram.AluInstruction) state inp =
    match command with
    | AluProgram.INP reg ->
        let x::xs = inp
        (changeReg state reg x), xs
    | AluProgram.ADD (l, r) ->
        let op1 = evaluateL state l
        let op2 = evaluateR state r
        (changeReg state l (op1 + op2)), inp
    | AluProgram.MUL (l, r) ->
        let op1 = evaluateL state l
        let op2 = evaluateR state r
        (changeReg state l (op1 * op2)), inp
    | AluProgram.DIV (l, r) ->
        let op1 = evaluateL state l
        let op2 = evaluateR state r
        (changeReg state l (op1 / op2)), inp
    | AluProgram.MOD (l, r) ->
        let op1 = evaluateL state l
        let op2 = evaluateR state r
        (changeReg state l (op1 % op2)), inp
    | AluProgram.EQL (l, r) ->
        let op1 = evaluateL state l
        let op2 = evaluateR state r
        (changeReg state l (if (op1 = op2) then 1 else 0), inp)

let runCode prog inp =
    let rec helper state prog inp =
        match prog with
        | x::xs ->
            let newState, newInput = execute x state inp
            //printfn "State: %O" newState
            helper newState xs newInput
        | [] -> state
    let state = { AluState.W=0; X=0; Y=0; Z=0 }
    helper state prog inp

let runCodeWithZ z prog inp =
    let rec helper state prog inp =
        match prog with
        | x::xs ->
            let newState, newInput = execute x state inp
            //printfn "State: %O" newState
            helper newState xs newInput
        | [] -> state
    let state = { AluState.W=0; X=0; Y=0; Z=z }
    helper state prog inp
    