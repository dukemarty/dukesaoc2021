namespace AoC.Utils

module MatrixOperations =

    let createValidNeighboursSimple pos width height =
        let r, c = pos

        let cands =
            [| (r, c - 1)
               (r, c + 1)
               (r - 1, c)
               (r + 1, c) |]

        cands
        |> Seq.filter
            (fun (r, c) ->
                (r >= 0)
                && (c >= 0)
                && (c < width)
                && (r < height))

    let createNeighboursWithDiagonals pos width height =
        let r, c = pos

        let cands =
            [| (r, c - 1)
               (r - 1, c - 1)
               (r - 1, c)
               (r - 1, c + 1)
               (r, c + 1)
               (r + 1, c + 1)
               (r + 1, c)
               (r + 1, c - 1) |]

        cands
        |> Seq.filter
            (fun (r, c) ->
                (r >= 0)
                && (c >= 0)
                && (c < width)
                && (r < height))
