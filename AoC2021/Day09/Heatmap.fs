
module Heatmap

open System
open AoC.Utils

type Heatmap = (int array) array

let loadHeatmap source: Heatmap =
    let rawData = DataInput.multipleRawLines source
    rawData |> Seq.map (fun r -> r.ToCharArray() |> Seq.map (fun c -> Int32.Parse (string c)) |> Array.ofSeq) |> Array.ofSeq

let getWidthHeight (heatmap: Heatmap) =
    let height = heatmap.Length
    let width = heatmap.[0].Length
    width, height
