module Graph


let parseGraph (lines: string list) =
    let findAllConnectedNodes k pairs =
        Seq.concat (
            seq {
                pairs |> Seq.filter (fun (n1, _) -> n1 = k) |> Seq.map (fun (_, n2) -> n2)
                pairs |> Seq.filter (fun (_, n2) -> n2 = k) |> Seq.map (fun (n1, _) -> n1)
            }
        )

    let pairs =
        lines
        |> Seq.map
            (fun l ->
                let tokens = l.Split("-")
                (tokens.[0], tokens.[1]))

    let keys =
        pairs
        |> Seq.collect
            (fun (n1, n2) ->
                seq {
                    n1
                    n2
                })
        |> Set.ofSeq

    keys
    |> Seq.map (fun k -> (k, findAllConnectedNodes k pairs))
    |> Map.ofSeq
