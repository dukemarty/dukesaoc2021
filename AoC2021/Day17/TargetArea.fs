
module TargetArea

open System
open System.Text.RegularExpressions
open AoC.Utils

type TargetArea = {
    Xfrom: int
    Xto: int
    Yfrom: int
    Yto: int
}

type HitResult = TooShort | InTarget | Overshot


let parseTargetArea line =
    let m = Regex.Match(line, "target area: x=(.+)\.\.(.+), y=(.+)\.\.(.+)")
    {
        Xfrom = Int32.Parse m.Groups.[1].Value
        Xto = Int32.Parse m.Groups.[2].Value
        Yfrom = Int32.Parse m.Groups.[3].Value
        Yto = Int32.Parse m.Groups.[4].Value
    }

let loadData source =
    let line = DataInput.singleRawLine source
    parseTargetArea line

let checkHitInX ta x =
    match x with
    | x when x < ta.Xfrom -> TooShort
    | x when (ta.Xfrom <= x) && (x <= ta.Xto) -> InTarget
    | _ -> Overshot

let checkHitInY ta y =
    match y with
    | y when y < ta.Yfrom -> TooShort
    | y when (ta.Yfrom <= y) && (y <= ta.Yto) -> InTarget
    | _ -> Overshot

