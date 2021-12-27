open System

String.init 10 (fun _ -> ".")

let freqs =
    [ 1; 2; 3 ]
    |> Seq.collect
        (fun i ->
            [ 1; 2; 3 ]
            |> Seq.collect (fun j -> [ 1; 2; 3 ] |> Seq.map (fun k -> i + j + k)))
    |> Seq.groupBy id
    |> Seq.map (fun (k, sq) -> (k, (sq |> List.ofSeq).Length))
    |> Map.ofSeq

freqs |> Map.toSeq |> Seq.map fst


let a = List.rev [1..14]
