let s = seq { for i in 1 .. 10 -> i}


let extractChar pos (numberStr: string) =
    numberStr.[pos]

extractChar 2 "00100"

open System

let countSingleBit numberList pos =
    numberList |> Seq.map (extractChar pos) |> Seq.map string |> Seq.map Int32.Parse |> Seq.sum
    

let part1PowerConsumption (lines: string list)  =
    let len = (List.head lines).Length
    let index = seq { for i in 0 .. len -> i}
    let t = countSingleBit lines
    index |> Seq.map t
    printfn "Power Consumption: %d" 0
    0




let a =  [ 5;1;2;3;5;3;4;6;4;5;7;4;2;5;3;5 ] 

let test n = n=5

let splitOn2 test lst =
    let isEmptyList l = l=[]
    List.skipWhile isEmptyList (    List.foldBack (fun el acc ->
        match acc with
        | [] when (test el) -> []
        | [] -> [[el]]
        | xs::ys when not (test el) -> (el::xs)::ys
        | _ -> []::acc
     )  lst [] )

splitOn2 test a


let splitOn test lst =
    List.foldBack (fun el lst ->
            match lst with
            | [] -> [[el]]
            | (x::xs)::ys when not (test el) -> (el::(x::xs))::ys
            | _ -> [el]::lst
         )  lst [] 

splitOn test a


let b=[ 1;2;5;2;3;1]

b |> List.filter (fun i -> not (i=2))


let d7s = "16,1,2,0,4,2,7,1,2,14"
let d7d = d7s.Split(',') |> Seq.map Int32.Parse |> List.ofSeq

let a1 = d7d |> Seq.map (fun i -> i - 1) |> List.ofSeq
let a2 = d7d |> Seq.map (fun i -> i - 2) |> List.ofSeq
let a3 = d7d |> Seq.map (fun i -> i - 3) |> List.ofSeq
let n1 = a1 |> Seq.filter (fun i -> i < 0) |> Seq.sum
let n2 = a2 |> Seq.filter (fun i -> i < 0) |> Seq.sum
let n3 = a3 |> Seq.filter (fun i -> i < 0) |> Seq.sum
let p1 = a1 |> Seq.filter (fun i -> i > 0) |> Seq.sum
let p2 = a2 |> Seq.filter (fun i -> i > 0) |> Seq.sum
let p3 = a3 |> Seq.filter (fun i -> i > 0) |> Seq.sum

let d7sm = float (d7d |> List.sum) / (float d7d.Length)
let d7qm = sqrt ( float (d7d |> Seq.map (fun i -> i*i) |> Seq.sum) / (float d7d.Length ) )

let d7map = d7d |> List.countBy id |> Map.ofList

d7map.Count / 2



type FoldDirection =
    | X
    | Y


