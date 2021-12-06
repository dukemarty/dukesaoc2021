namespace AoC.Utils

module DataProcessing =

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
    
