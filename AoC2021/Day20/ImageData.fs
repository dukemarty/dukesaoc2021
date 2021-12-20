module ImageData

open System
open AoC.Utils

let loadFilterAndImage source =
    let inputBlocks =
        DataInput.multipleRawLines source
        |> DataProcessing.splitListOnElem DataProcessing.isEmptyLine

    let filter = inputBlocks.[0].[0]
    let image = inputBlocks.[1] |> Array.ofList
    filter, image

let extendImageTriple (baseImage: string array) =
    let constDots l = String.init l (fun _ -> ".")
    let height = baseImage.Length
    let width = baseImage.[0].Length

    let res =
        (List.init height (fun _ -> constDots (3 * width)))
        @ (baseImage
           |> Array.map
               (fun s ->
                   String.concat
                       ""
                       [ (constDots width)
                         s
                         (constDots width) ])
           |> List.ofArray)
          @ (List.init height (fun _ -> constDots (3 * width)))
        |> Array.ofList

    res

let extractWindow3x3 (image: string array) r c =
    String.concat
        ""
        [ image.[r - 1].[c - 1..c + 1]
          image.[r].[c - 1..c + 1]
          image.[r + 1].[c - 1..c + 1] ]

let mapToBinNumber (s: string) =
    Convert.ToInt32(
        System.String(
            s.ToCharArray()
            |> Seq.map (fun c -> if (c = '#') then '1' else '0')
            |> Array.ofSeq
        ),
        2
    )

let applyFilter (filter: string) (image: string array) =
    let height = image.Length
    let width = image.[0].Length

    seq { for i in 1 .. (height - 2) -> i }
    |> Seq.map
        (fun r ->
            seq { for j in 1 .. (width - 2) -> j }
            |> Seq.map (fun c -> extractWindow3x3 image r c)
            |> Seq.map mapToBinNumber
            |> Seq.map (fun i -> filter.[i])
            |> Array.ofSeq
            |> System.String)
    |> Array.ofSeq


let countLitPixels (image: string array) =
    Array.sumBy (fun r -> (String.filter (fun c -> c = '#') r).Length) image
