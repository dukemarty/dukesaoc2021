// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let part1LargestValidMonadTry1 prog =
    for i in 1..9 do
        for j in 1..9 do
            for k in 1..9 do
                for l in 1..9 do
                    for m in 1..9 do
                        for n in 1..9 do
                            for o in 1..9 do
                                for p in 1..9 do
                                    for q in 1..9 do
                                        for r in 1..9 do
                                            for s in 1..9 do
                                                for t in 1..9 do
                                                    for u in 1..9 do
                                                        for v in 1..9 do
                                                            //let resState = AluSimulator.runCode prog [ 1;1;1;1;1;1;1;1;1; i;j;k;l;m ]
                                                            let inputList = [ 10-i;10-j;10-k;10-l;10-m;10-n;10-o;10-p;10-q;10-r;10-s;10-t;10-u;10-v ]
                                                            let resState = AluSimulator.runCode prog inputList
                                                            if (resState.Z = 0) then
                                                                printfn "  Valid with :  %A" inputList
                                                            //else
                                                            //    printfn "  Invalid with i=%d, j=%d" i j
    //printfn "  Result state: %O" resState
    let res = ""
    printfn "Part 1, result: %s" res

let splitProgramChunks (prog: AluProgram.AluInstruction list) =
    let rec helper prog =
        let _::xs = prog
        let splitPoint = xs |> List.tryFindIndex (fun inst -> match inst with
                            | AluProgram.INP _ -> true
                            | _ -> false)
        match splitPoint with
        | Some i -> prog.[..i]::(helper prog.[(i+1)..])
        | None -> [prog]

    helper prog

let analyzeTry1 prog chunk =
    let chunks = splitProgramChunks prog
    for c in 0..13 do
        printfn "=== Chunk %d =================================" (c+1)
        for i in 1..79 do
            let inputList = [ i ]
            //let resState = AluSimulator.runCode chunks.[chunk-1] inputList
            let resState = AluSimulator.runCode chunks.[c] inputList
            printfn "  Z for %d:  %d" i resState.Z
        printfn "---------------------------------------------------------"

let analyzeTry2 prog chunk =
    let chunks = splitProgramChunks prog
    for c in 0..13 do
        printfn "=== Chunk %d =================================" (c+1)
        for z in 0..53 do
            for i in 1..9 do
                let inputList = [ i ]
                //let resState = AluSimulator.runCode chunks.[chunk-1] inputList
                let resState = AluSimulator.runCodeWithZ z chunks.[c] inputList
                printfn "  new Z for z=%d/i=%d:  %d" z i resState.Z
            printfn "---------------------------------------------------------"

let analyzeTry3 prog =
    let chunks = splitProgramChunks prog
    let mutable validResultZs = Map.empty
    validResultZs <- validResultZs.Add(13, Set.empty.Add(0))
    let mutable possibleInZs = Map.empty
    for c in (List.rev [0..13]) do
        printfn "=== Chunk %d =================================" (c+1)
        let mutable possibleZs = Set.empty
        for z in 0..1000000 do
            for i in 1..9 do
                let inputList = [ i ]
                //let resState = AluSimulator.runCode chunks.[chunk-1] inputList
                let resState = AluSimulator.runCodeWithZ z chunks.[c] inputList
                if (validResultZs.[c].Contains resState.Z) then
                    //printfn "  Z is %d for z=%d/i=%d" resState.Z z i 
                    possibleZs <- possibleZs.Add (z,i, resState.Z)
            //printfn "---------------------------------------------------------"
        possibleInZs <- possibleInZs.Add(c, possibleZs)
        validResultZs <- validResultZs.Add(c-1, possibleZs |> Set.map (fun (z, _, _) -> z))
        //printfn "  Z's which allow to achieve the required results:"
        //for a in (List.splitInto 5 (possibleZs |> Set.toList)) do
        //    printfn "    %A" a
    // collect results
    let mutable res = [possibleInZs.[0] |> Set.toList |> List.maxBy (fun (_, i, _) -> i)]
    for i in [1..13] do
        let (_, _, prev) = res.Head
        let nexts = possibleInZs.[i] |> Set.filter (fun (z, _, _) -> z = prev) |> Set.toList
        res <- (nexts |> List.maxBy (fun (_, i, _) -> i))::res
    printfn "Analysis, one result: %A" res
    let resultIs = res |> List.map (fun (_, i, _) -> i) |> List.rev
    printfn "  Resulting i's: %A" resultIs
    printfn "  As string    : %s" (String.concat "" (resultIs |> Seq.map (fun i -> i.ToString())))
    //Analysis, one result: [(11, 4, 0); (310, 9, 11); (8076, 9, 310); (310, 7, 8076); (8072, 9, 310);
    // (310, 9, 8072); (8073, 9, 310); (209919, 5, 8073); (8073, 9, 209919);
    // (209907, 1, 8073); (8073, 9, 209907); (310, 4, 8073); (11, 8, 310); (0, 9, 11)]
    
    //Analysis, one result: [(8, 1, 0); (225, 2, 8); (5860, 3, 225); (225, 1, 5860); (5854, 1, 225);
    //(225, 1, 5854); (5860, 6, 225); (152377, 1, 5860); (5860, 5, 152377);
    //(152369, 1, 5860); (5860, 9, 152369); (225, 1, 5860); (8, 1, 225); (0, 6, 8)]

let analyzeTry4 prog =
    let digitListToString l = String.concat "" (l |> List.map (fun i -> i.ToString()))

    let findMaxResult (posZs: Map<int, Set<int*int*int>>) =
        let mutable res = [posZs.[0] |> Set.toList |> List.maxBy (fun (_, i, _) -> i)]
        for i in [1..13] do
            let (_, _, prev) = res.Head
            let nexts = posZs.[i] |> Set.filter (fun (z, _, _) -> z = prev) |> Set.toList
            res <- (nexts |> List.maxBy (fun (_, i, _) -> i))::res
        printfn "Analysis, one result: %A" res
        let resultIs = res |> List.map (fun (_, i, _) -> i) |> List.rev
        res, resultIs

    let findMinResult (posZs: Map<int, Set<int*int*int>>) =
        let mutable res = [posZs.[0] |> Set.toList |> List.minBy (fun (_, i, _) -> i)]
        for i in [1..13] do
            let (_, _, prev) = res.Head
            let nexts = posZs.[i] |> Set.filter (fun (z, _, _) -> z = prev) |> Set.toList
            res <- (nexts |> List.minBy (fun (_, i, _) -> i))::res
        printfn "Analysis, one result: %A" res
        let resultIs = res |> List.map (fun (_, i, _) -> i) |> List.rev
        res, resultIs

    let chunks = splitProgramChunks prog
    let mutable validResultZs = Map.empty
    validResultZs <- validResultZs.Add(13, Set.empty.Add(0))
    let mutable possibleInZs = Map.empty
    for c in (List.rev [0..13]) do
        printfn "=== Chunk %d =================================" (c+1)
        let mutable possibleZs = Set.empty
        for z in 0..1000000 do
            for i in 1..9 do
                let inputList = [ i ]
                let resState = AluSimulator.runCodeWithZ z chunks.[c] inputList
                if (validResultZs.[c].Contains resState.Z) then
                    possibleZs <- possibleZs.Add (z,i, resState.Z)
        possibleInZs <- possibleInZs.Add(c, possibleZs)
        validResultZs <- validResultZs.Add(c-1, possibleZs |> Set.map (fun (z, _, _) -> z))
    let maxRes, maxIs = findMaxResult possibleInZs
    printfn "Analysis #4, results for max:"
    printfn "  resulting I's list  for max: %A" maxIs
    printfn "  resulting I's MonaD for max: %s" (digitListToString maxIs)
    let minRes, minIs = findMinResult possibleInZs
    printfn "Analysis #4, results for min:"
    printfn "  resulting I's list  for min: %A" minIs
    printfn "  resulting I's MonaD for min: %s" (digitListToString minIs)
    //Analysis, one result: [(11, 4, 0); (310, 9, 11); (8076, 9, 310); (310, 7, 8076); (8072, 9, 310);
    // (310, 9, 8072); (8073, 9, 310); (209919, 5, 8073); (8073, 9, 209919);
    // (209907, 1, 8073); (8073, 9, 209907); (310, 4, 8073); (11, 8, 310); (0, 9, 11)]
    //Analysis #4, results for max:
    //  resulting I's list  for max: [9; 8; 4; 9; 1; 9; 5; 9; 9; 9; 7; 9; 9; 4]
    //  resulting I's MonaD for max: 98491959997994
    //Analysis, one result: [(8, 1, 0); (225, 2, 8); (5860, 3, 225); (225, 1, 5860); (5854, 1, 225);
    // (225, 1, 5854); (5860, 6, 225); (152377, 1, 5860); (5860, 5, 152377);
    // (152369, 1, 5860); (5860, 9, 152369); (225, 1, 5860); (8, 1, 225); (0, 6, 8)]
    //Analysis #4, results for min:
    //  resulting I's list  for min: [6; 1; 1; 9; 1; 5; 1; 6; 1; 1; 1; 3; 2; 1]
    //  resulting I's MonaD for min: 61191516111321

let part1LargestValidMonad prog =
    //let resState = AluSimulator.runCode prog [ 1;1;1;1;1;1;1;1;1; i;j;k;l;m ]
    for i in 1..9 do
        let v = 10 - i
        let inputList = [ 1;9; 9;9; 9;9; 7;9; v;9; 9;v; 9;9 ]
        let resState = AluSimulator.runCode prog inputList
        printfn "  Z for %d:  %d" v resState.Z
    //else
    //    printfn "  Invalid with i=%d, j=%d" i j
    //printfn "  Result state: %O" resState
    let res = ""
    printfn "Part 1, result: %s" res

let part1JustCheck prog =
    let inputList = [9; 8; 4; 9; 1; 9; 5; 9; 9; 9; 7; 9; 9; 4]
    let resState = AluSimulator.runCode prog inputList
    printfn "Part 1, result state: %O" resState
    printfn "  so result is: %s" (String.concat "" (inputList |> Seq.map (fun i -> i.ToString())))


[<EntryPoint>]
let main argv =
    printfn "Day 24: Arithmetic Logic Unit\n=============================\n"
    let aluProg = (AluProgram.loadProgram (DataInput.Puzzle)) |> List.ofSeq
    printfn "ALU program: %A" aluProg
    analyzeTry4 aluProg
    //part1JustCheck aluProg
    //part1LargestValidMonad aluProg
    0 // return an integer exit code
