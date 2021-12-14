
module PolymerizationInput

open System
open System.Text.RegularExpressions
open AoC.Utils

let private rawData source =
    DataInput.multipleRawLines source
    |> DataProcessing.splitListOnElem DataProcessing.isEmptyLine


let parseRules (data: string list) =
    let parseRule rawRule =
        let m = Regex.Match(rawRule, "^(\w\w)\s*->\s*(\w)$")
        m.Groups.[1].Value, m.Groups.[2].Value
    data |> Seq.map parseRule |> Map.ofSeq

let loadData source =
    let data = rawData source

    match data with
    | (template::_) :: rules :: _ -> template.Trim(), (parseRules rules)
    | _ -> failwith "Input data has incorrect format (not enough data sections)"

