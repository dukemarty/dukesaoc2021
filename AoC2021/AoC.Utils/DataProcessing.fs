namespace AoC.Utils

module DataProcessing =

    open System

    let isEmptyLine s = String.IsNullOrWhiteSpace s


    let splitListOnElem test lst =
        List.skipWhile
            (fun l -> l = [])
            (List.foldBack
                (fun el acc ->
                    match acc with
                    | [] when (test el) -> []
                    | [] -> [ [ el ] ]
                    | xs :: ys when not (test el) -> (el :: xs) :: ys
                    | _ -> [] :: acc)
                lst
                [])
    
    let parse3dPos (s: string) =
        s.Split(',') |> Seq.map Int32.Parse |> Array.ofSeq