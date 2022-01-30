module Search

let MovementCostByType =
    Map.empty.
        Add('A', 1).
        Add('B', 10).
        Add('C', 100).
        Add('D', 1000)

type Amphipod = {
    Id: int
    Type: char
    Location: int }

type SearchNode =
    { Id: int
      State: Amphipod array
      Estimation: int
      AccumulatedCost: int
      LastMovedLoc: int option
      PreState: int option }

type SearchState = {
    Nodes: Map<int, SearchNode>
    VisitedStates: Set<Amphipod array>
    Unexplored: Set<SearchNode>
    }

type Solution =
    { Steps: (BurrowState.Amphipod list) list
      Cost: int }

let private appraiseAmphipod (a: Amphipod) =
    let targetLoc = BurrowState.targetsForAmphipods.[a.Type].MinimumElement
    let targetX, targetY = BurrowState.locationPositions.[targetLoc]
    let currX, currY = BurrowState.locationPositions.[a.Location]
    if (BurrowState.corridorLocations.Contains a.Location) then
        ((abs (currX - targetX)) + 1) * MovementCostByType.[a.Type]
    else
        ((abs (currX - targetX)) + 2) * MovementCostByType.[a.Type]

let appraiseState (st: Amphipod array) =
    Seq.sumBy appraiseAmphipod st

let selectMovableAmphipods (node: SearchNode) =
    let endPositionReachable (a: Amphipod) =
        let endPositions = BurrowState.targetsForAmphipods.[a.Type]
        let inPos, endPos = endPositions.MinimumElement, endPositions.MaximumElement
        if ((node.State |> Array.tryFind (fun b -> (b.Location = endPos) && (not (b.Type = a.Type)))).IsSome) then
            false
        else if ((node.State |> Array.tryFind (fun b -> (b.Location = inPos))).IsSome) then
            false
        else if (
            BurrowState.positionsBetween a.Location (inPos-1)
            |> List.map (fun n -> (node.State |> Array.tryFind (fun b -> b.Location = n)).IsSome)
            |> List.fold (fun l r -> l || r) false
            ) then
            false
        else
            true
    let isMovingAllowed (a: Amphipod) =
        if (node.LastMovedLoc.IsSome && a.Location = node.LastMovedLoc.Value) then
            true
        else if (not (BurrowState.corridorLocations.Contains a.Location)) then
            true
        else if ((BurrowState.corridorLocations.Contains a.Location) && (endPositionReachable a)) then
            true
        else
            false
    let checkAmphipodMovable (a: Amphipod) =
        let validDestinations = BurrowState.reachableFromLocation.[a.Location]
                                |> List.filter (fun l -> not (Array.exists (fun (s: Amphipod) -> s.Location = l) node.State))
                                |> List.filter (fun l -> match (a.Location, l) with
                                                             | (s, _) when (not (BurrowState.corridorLocations.Contains s)) -> true
                                                             | (_, d) when (BurrowState.corridorLocations.Contains d) -> true
                                                             | (s, d) when ((BurrowState.corridorLocations.Contains s) && (BurrowState.targetsForAmphipods.[a.Type].Contains d)) -> true
                                                             | _ -> false
                                                             )
        validDestinations.Length > 0
            
    node.State
    |> Array.filter isMovingAllowed
    |> Array.filter checkAmphipodMovable
    |> Array.toList

let createSuccessors (searchState: SearchState) (node: SearchNode) =
    let createValidSuccesors (a: Amphipod) =
        let reachableStates =
            BurrowState.reachableFromLocation.[a.Location]
            |> List.filter (fun l -> not (Array.exists (fun (s: Amphipod) -> s.Location = l) node.State))
            |> List.filter (fun l -> match (a.Location, l) with
                | (s, _) when (not (BurrowState.corridorLocations.Contains s)) -> true
                | (_, d) when (BurrowState.corridorLocations.Contains d) -> true
                | (s, d) when ((BurrowState.corridorLocations.Contains s) && (BurrowState.targetsForAmphipods.[a.Type].Contains d)) -> true
                | _ -> false
                )
            |> List.map (fun l ->
                            let newState = Array.copy node.State
                            newState.[a.Id] <- { a with Location = l }
                            (l, newState, node.AccumulatedCost + MovementCostByType.[a.Type]))

        let unseenReachableState = reachableStates |> List.filter (fun (_, state, _) -> not (searchState.VisitedStates.Contains state))
        //if ((searchState.Nodes |> Map.count > 990) && (searchState.Nodes |> Map.count < 1010)) then
        //    let alreadySeenStates = reachableStates |> List.filter (fun (_, state, _) -> searchState.VisitedStates.Contains state)
        //    if (alreadySeenStates.Length > 0) then
        //        for (_, ass, _) in alreadySeenStates do
        //            printfn "  Already seen: %A" ass
        //        printfn "Number of already seen states: %d" (alreadySeenStates |> List.length)
        unseenReachableState

    let movableAs = if (node.LastMovedLoc.IsSome  && BurrowState.notWaitLocations.Contains(node.LastMovedLoc.Value)) then
                        [ (node.State |> Array.find (fun a -> a.Location = node.LastMovedLoc.Value)) ]
                    else 
                        selectMovableAmphipods node
    
    let newNodes = movableAs
                    |> List.collect (createValidSuccesors)
                    |> List.mapi (fun i (l, state, cost) ->
                                    { node with
                                        Id = searchState.Nodes.Count + i
                                        State = state
                                        AccumulatedCost = cost
                                        Estimation = appraiseState state
                                        LastMovedLoc = Some l
                                        PreState = Some node.Id })
    let newNodesMap = newNodes |> List.fold (fun (s: Map<int, SearchNode>) n -> s.Add(n.Id, n)) searchState.Nodes
    let newVisited = newNodes |> List.fold (fun (s: Set<Amphipod array>) n -> s.Add(n.State)) searchState.VisitedStates
    let newUnexplored = newNodes |> List.fold (fun (s: Set<SearchNode>) n -> s.Add(n)) (searchState.Unexplored.Remove node)
    { Nodes=newNodesMap; VisitedStates=newVisited; Unexplored=newUnexplored }, newNodes

let private prettifyNode (node: SearchNode) =
    let prettifyAmphis (a: Amphipod) =
        sprintf "Amph%d(%c@%d)" a.Id a.Type a.Location
    sprintf "Loc's: [%A] for %d/+%d" (node.State |> Array.map prettifyAmphis) node.AccumulatedCost node.Estimation

let isFinalState (node: SearchNode) =
    node.State |> Array.forall (fun a -> BurrowState.targetsForAmphipods.[a.Type].Contains a.Location)

// always assuming that no improved routes to previously found state are possible
let rec runAStar (searchState: SearchState) =
    let nextNode = searchState.Unexplored |> Seq.minBy (fun sn -> sn.AccumulatedCost + sn.Estimation)
    //printfn "  Expanding: %s" (prettifyNode nextNode)
    printfn " %5d/%5d -> %5d " nextNode.AccumulatedCost nextNode.Estimation (nextNode.AccumulatedCost + nextNode.Estimation)
    let updatedState, newNodes = createSuccessors searchState nextNode
    if (searchState.VisitedStates |> Set.count = 10000) then
        printfn "Sizes: |VisitedStates|=%d   |Nodes|=%d    |Unexplored|=%d" (searchState.VisitedStates |> Set.count) (searchState.Nodes |> Map.count) (searchState.Unexplored |> Set.count)
        printfn "  New generated nodes:\n    %s" (String.concat "\n    " (newNodes |> List.map prettifyNode))
        failwith "bla"
    //printfn "  New generated nodes:\n    %s" (String.concat "\n    " (newNodes |> List.map prettifyNode))
    let finalNodes = newNodes |> List.filter isFinalState
    match finalNodes with
    | [] -> runAStar updatedState
    | l -> l |> List.minBy (fun n -> n.AccumulatedCost)

let runAStarSearch (state: BurrowState.Amphipod list) =
    let searchAmphipods = state |> List.mapi (fun i a -> { Id=i; Type=a.Type; Location=a.Location }) |> List.toArray
    let initialSearchNode =
        { Id = 0
          State = searchAmphipods
          AccumulatedCost = 0
          Estimation = appraiseState searchAmphipods
          LastMovedLoc = None
          PreState = None }
    let initialSearchState = {
        Nodes=Map.empty.Add(0, initialSearchNode)
        VisitedStates=([initialSearchNode.State] |> Set.ofList)
        Unexplored=[initialSearchNode] |> Set.ofList}

    runAStar initialSearchState

    //{ Steps = [ state ]; Cost = 0 }
