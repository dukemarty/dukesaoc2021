module RebootSteps

open System
open System.Text.RegularExpressions
open AoC.Utils

type RebootAction =
    | On
    | Off

type Range = { From: int; To: int }

type RebootStep =
    { Action: RebootAction
      X: Range
      Y: Range
      Z: Range }


let parseRebootStep line =
    let m =
        Regex.Match(line, "(\w+)\s+x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)")

    let action =
        match m.Groups.[1].Value with
        | "on" -> On
        | "off" -> Off
        | s -> failwithf "Unknown action found: %s" s

    { Action = action
      X =
          { From = (Int32.Parse m.Groups.[2].Value)
            To = (Int32.Parse m.Groups.[3].Value) }
      Y =
          { From = (Int32.Parse m.Groups.[4].Value)
            To = (Int32.Parse m.Groups.[5].Value) }
      Z =
          { From = (Int32.Parse m.Groups.[6].Value)
            To = (Int32.Parse m.Groups.[7].Value) } }

let loadAllSteps source =
    let lines = DataInput.multipleRawLines source
    lines |> List.map parseRebootStep
