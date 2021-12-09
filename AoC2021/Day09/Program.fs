// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let createExistingNeighboursPart1 data r c =
    let w, h = Heatmap.getWidthHeight data

    let cands =
        [| (c - 1, r)
           (c + 1, r)
           (c, r - 1)
           (c, r + 1) |]

    cands
    |> Seq.filter (fun (x, y) -> (x >= 0) && (y >= 0) && (x < w) && (y < h))

let isLowPoint data r c =
    //let w, h = Heatmap.getWidthHeight data
    //let cands = [| (c-1, r); (c+1,r); (c,r-1); (c,r+1) |]
    //let neighbours = cands |> Seq.filter (fun (x, y) -> (x >= 0) && (y >=0) && (x < w) && (y < h))
    let neighbours = createExistingNeighboursPart1 data r c
    //printfn "Neighbour positions for (%d, %d): %A" r
    let countLowerEqualNeighbours =
        neighbours
        |> Seq.map
            (fun (x, y) ->
                if data.[r].[c] < data.[y].[x] then
                    0
                else
                    1)
        |> Seq.sum

    countLowerEqualNeighbours = 0

let findLowPoints data =
    let w, h = Heatmap.getWidthHeight data

    seq { for i in 0 .. (h - 1) -> i }
    |> Seq.map
        (fun r ->
            seq { for j in 0 .. (w - 1) -> j }
            |> Seq.filter (fun c -> isLowPoint data r c)
            |> Seq.map (fun c -> (r, c)))
    |> Seq.concat

let part1RisklevelSum data =
    let lowPoints = findLowPoints data |> List.ofSeq
    printfn "  %d low points : %A" lowPoints.Length lowPoints

    let riskLevels =
        lowPoints
        |> Seq.map (fun (r, c) -> data.[r].[c] + 1)
        |> List.ofSeq
    //printfn "  Risk levels: %A" riskLevels
    let res = riskLevels |> Seq.sum
    printfn "Part 1, sum of risk levels: %d" res
    lowPoints

let createExistingNeighboursPart2 data r c =
    let w, h = Heatmap.getWidthHeight data

    let cands =
        [| (r - 1, c)
           (r + 1, c)
           (r, c - 1)
           (r, c + 1) |]

    cands
    |> Seq.filter (fun (x, y) -> (x >= 0) && (y >= 0) && (x < h) && (y < w))


let rec growBasin (data: Heatmap.Heatmap) unexploredList (unexploredSet: Set<int * int>) (basin: Set<int * int>) =
    match unexploredList with
    | x :: xs ->
        let r, c = x

        let neighbours =
            createExistingNeighboursPart2 data r c
            |> Seq.filter (fun p -> not (unexploredSet.Contains(p)))
            |> Seq.filter (fun p -> not (basin.Contains(p)))
            |> Seq.filter (fun (r, c) -> data.[r].[c] <> 9)
            |> List.ofSeq

        growBasin data (xs @ neighbours) (Set.union (unexploredSet.Remove x) (set neighbours)) (basin.Add x)
    | [] -> basin

let determineBasins data (lp: int * int) =
    growBasin data [ lp ] (set [ lp ]) Set.empty

let part2BiggestBasins data lowPoints =
    let allBasins =
        lowPoints |> Seq.map (determineBasins data)

    let sortedBasins =
        allBasins
        |> Seq.sortByDescending (fun b -> b.Count)

    printfn "Basins: %A" (sortedBasins |> List.ofSeq)
    let basins = Seq.take 3 sortedBasins
    let basinSizes = basins |> Seq.map (fun b -> b.Count)
    let res = Seq.reduce (fun a b -> a * b) basinSizes
    printfn "Part 2, product of 3 biggest basin's sizes: %d (= \prod (%A) )" res basinSizes

[<EntryPoint>]
let main argv =
    printfn "Day 9: Smoke Basin\n==================\n"
    let data = Heatmap.loadHeatmap DataInput.Puzzle
    let lowPoints = part1RisklevelSum data
    printfn "  Low points: %A" lowPoints
    part2BiggestBasins data lowPoints
    0 // return an integer exit code
