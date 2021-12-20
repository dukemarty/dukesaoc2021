module ScannerReadings

open System
open AoC.Utils

let private parseScannerBlock block =
    List.tail block |> List.map DataProcessing.parse3dPos

let loadData source =
    let blocks =
        DataInput.multipleRawLines source
        |> DataProcessing.splitListOnElem DataProcessing.isEmptyLine

    blocks |> List.map parseScannerBlock
