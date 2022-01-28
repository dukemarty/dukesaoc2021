module RebootSteps

open System
open System.Text.RegularExpressions
open AoC.Utils

type RebootAction =
    | On
    | Off

type RebootStep =
    { Action: RebootAction
      X: Cuboid.Range
      Y: Cuboid.Range
      Z: Cuboid.Range }

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

//let getCuboid rs =
//    { Cuboid.X = rs.X
//      Cuboid.Y = rs.Y
//      Cuboid.Z = rs.Z }

let getCuboid rs = [| rs.X; rs.Y; rs.Z |]