// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

type State =
    { PlayerPos: int array
      PlayerPoints: int64 array
      Die: int
      NextPlayer: int
      ThrowCount: int }

type State2 =
    { PlayerPos: int array
      PlayerPoints: int array
      PossibleWaysToHere: int64
      NextPlayer: int }


let progressDie d s = ((d + s - 1) % 100) + 1

let progressPos p sw = ((p + sw - 1) % 10) + 1

let part2Frequencies =
    [ 1; 2; 3 ]
    |> Seq.collect
        (fun i ->
            [ 1; 2; 3 ]
            |> Seq.collect (fun j -> [ 1; 2; 3 ] |> Seq.map (fun k -> i + j + k)))
    |> Seq.groupBy id
    |> Seq.map (fun (k, sq) -> (k, (sq |> List.ofSeq).Length))
    |> Map.ofSeq

let part2PossibleThrows = part2Frequencies |> Map.toSeq |> Seq.map fst

let rec part1RunGameTillPoints (state: State) targetPoints =
    let throw =
        state.Die
        + (progressDie state.Die 1)
        + (progressDie state.Die 2)

    let newThrowCount = state.ThrowCount + 3
    let newDie = progressDie state.Die 3

    let newPos =
        progressPos state.PlayerPos.[state.NextPlayer] throw

    let newPoints =
        state.PlayerPoints.[state.NextPlayer]
        + (int64 newPos)

    if (newPoints >= targetPoints) then
        let otherPlayerPoints =
            state.PlayerPoints.[1 - state.NextPlayer]

        otherPlayerPoints * (int64 newThrowCount)
    else
        let newState =
            { state with
                  Die = newDie
                  NextPlayer = 1 - state.NextPlayer
                  ThrowCount = newThrowCount }

        newState.PlayerPos.[state.NextPlayer] <- newPos
        newState.PlayerPoints.[state.NextPlayer] <- newPoints
        //printfn "-----------------\n%O\n--------------------" newState
        part1RunGameTillPoints newState targetPoints

let part1WinnerAt1000 (data: int array) =
    let state =
        { PlayerPos = [| data.[0]; data.[1] |]
          PlayerPoints = [| 0L; 0L |]
          Die = 1
          NextPlayer = 0
          ThrowCount = 0 }

    printfn "Intial state:\n%O\n=====================" state
    let res = part1RunGameTillPoints state 1000L
    printfn "Part 1, loser-points * throws: %d" res

let part2RunGameTillPoints (state: State2) targetPoints =
    let rec makeThrow s =
        //printfn "-----------------\n%O\n--------------------" s
        let x =
            part2PossibleThrows
            |> Seq.map (fun t ->
                let newPos = progressPos s.PlayerPos.[s.NextPlayer] t
                let newPoints = s.PlayerPoints.[s.NextPlayer] + newPos
                let possibleWaysHere = s.PossibleWaysToHere * (int64 part2Frequencies.[t])
                if (newPoints >= targetPoints) then
                    if (s.NextPlayer = 0) then
                        [| possibleWaysHere; 0L |]
                    else
                        [| 0L; possibleWaysHere |]
                else
                    let newState =
                        { s with
                            PlayerPos = Array.copy s.PlayerPos
                            PlayerPoints = Array.copy s.PlayerPoints
                            NextPlayer = 1 - s.NextPlayer
                            PossibleWaysToHere = possibleWaysHere }
                    newState.PlayerPos.[s.NextPlayer] <- newPos
                    newState.PlayerPoints.[s.NextPlayer] <- newPoints
                    makeThrow newState
                )
        //printfn "  Intermediate results: %A" (x |> List.ofSeq)
        x |> Seq.reduce (fun a b -> [| (a.[0]+b.[0]); (a.[1]+b.[1]) |])
    makeThrow state

let part2WinnerPossibilities (data: int array) =
    let state =
        { PlayerPos = [| data.[0]; data.[1] |]
          PlayerPoints = [| 0; 0 |]
          PossibleWaysToHere = 1L
          NextPlayer = 0 }
    
    printfn "Intial state:\n%O\n=====================" state
    let res = part2RunGameTillPoints state 21
    printfn "  res[0]: %d" res.[0]
    printfn "  res[1]: %d" res.[1]
    let winner = if (res.[0]>res.[1]) then res.[0] else res.[1]
    printfn "Part 2, possibilities for winner: %d" winner

[<EntryPoint>]
let main argv =
    printfn "Day 21: Dirac Dice\n==================\n"

    let data =
        DataInput.multipleRawLines DataInput.Puzzle
        |> Seq.map
            (fun (s: string) ->
                let tokens = s.Split(' ')
                Int32.Parse(tokens.[tokens.Length - 1]))
        |> Array.ofSeq

    part1WinnerAt1000 data
    part2WinnerPossibilities data
    0 // return an integer exit code
