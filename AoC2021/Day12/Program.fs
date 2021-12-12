// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

let findAllPathsPart1 (graph: Map<string, string seq>) startNode (endNode: string) =
    let rec deepSearch pos path (visited: Set<string>) =
        let nextNodes = graph.[pos] |> Seq.filter (fun s -> not (visited.Contains s))
        let mutable newPaths: string list list = []
        for n in nextNodes do
            match n with
            | a when a=endNode -> newPaths <- (endNode::path)::newPaths
            | a when Char.IsUpper(a.[0]) ->
                let addPaths = deepSearch a (a::path) visited
                newPaths <- (List.append newPaths addPaths)
            | a ->
                let addPaths = deepSearch a (a::path) (visited.Add a)
                newPaths <- (List.append newPaths addPaths)
        newPaths

    deepSearch startNode [startNode] (Set<string>(seq { startNode }))

let part1CountPossibleWays (graph: Map<string, string seq>) =
    let allPaths = findAllPathsPart1 graph "start" "end"
    //printfn "  Found paths: %A" allPaths
    printfn "Part 1, #paths: %d" allPaths.Length


let findAllPathsPart2 (graph: Map<string, string seq>) startNode (endNode: string) =
    let rec deepSearch pos path (visited: Set<string>) (doubleVisited: bool) =
        let nextNodes = graph.[pos] |> Seq.filter (fun s -> not (visited.Contains s))
        let mutable newPaths: string list list = []
        for n in nextNodes do
            match n with
            | a when a=endNode -> newPaths <- (endNode::path)::newPaths
            | a when Char.IsUpper(a.[0]) ->
                let addPaths = deepSearch a (a::path) visited doubleVisited
                newPaths <- (List.append newPaths addPaths)
            | a ->
                let addPaths1 =
                    if (not doubleVisited) then
                        deepSearch a (a::path) visited true
                    else
                        []
                let addPaths2 = deepSearch a (a::path) (visited.Add a) doubleVisited
                let merged = Set.union (Set.ofList addPaths1) (Set.ofList addPaths2)
                newPaths <- (List.append newPaths (Set.toList merged))
        newPaths

    deepSearch startNode [startNode] (Set<string>(seq { startNode })) false

let part2CountPossibleWays (graph: Map<string, string seq>) =
    let allPaths = findAllPathsPart2 graph "start" "end"
    //printfn "  Found paths: %A" allPaths
    printfn "Part 2, #paths: %d" allPaths.Length
    
[<EntryPoint>]
let main argv =
    printfn "Day 12: Passage Pathing\n=======================\n"
    let graph = DataInput.multipleRawLines (DataInput.Puzzle) |> Graph.parseGraph
    printfn "Graph: %A" graph
    part1CountPossibleWays graph
    part2CountPossibleWays graph
    0 // return an integer exit code
