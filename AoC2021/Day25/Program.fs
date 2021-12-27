// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let stepCucumbers (data: char array array) =
    let w = data.[0].Length
    let h = data.Length
    let mutable changeCount = 0
    let stepRight row =
        row |> Array.mapi (fun i c -> 
                                match (row.[(i+w-1)%w], c, row.[(i+1)%w]) with
                                | ('>', '.', _) ->
                                    changeCount <- changeCount + 1
                                    '>'
                                | (_, '>', '.') ->
                                    changeCount <- changeCount + 1
                                    '.'
                                | _ -> c)
    let stepDown matrix =
        matrix |> Array.mapi (fun ri row ->
                               row |> Array.mapi (fun ci c ->
                                                    match (matrix.[(ri-1+h)%h].[ci], c, matrix.[(ri+1)%h].[ci]) with
                                                    | ('v', '.', _) ->
                                                        changeCount <- changeCount + 1
                                                        'v'
                                                    | (_, 'v', '.') ->
                                                        changeCount <- changeCount + 1
                                                        '.'
                                                    | _ -> c
                                                    ))

    stepDown (data |> Array.map stepRight), changeCount

let prettyPrint (data: char array array) =
    printfn "%s" (String.concat "\n" (data |> Array.map (fun r -> System.String r)))
    printfn "----------------------------------------"

let part1RunTillFixstate data =
    let rec helper data count =
        let step, changeCount = stepCucumbers data
        //prettyPrint step
        if (changeCount > 0) then
            helper step (count + 1)
        else
            data, count
    let finalState, resCount = helper data 1
    printfn "Part 1, #steps till fix state: %d" resCount

[<EntryPoint>]
let main argv =
    printfn "Day 25: Sea Cucumber\n====================\n"

    let data =
        DataInput.multipleRawLines DataInput.Puzzle
        |> List.map (fun s -> s.ToCharArray()) |> List.toArray

    part1RunTillFixstate data

    0 // return an integer exit code
