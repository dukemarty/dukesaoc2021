// Learn more about F# at http://fsharp.org

open System
open AoC.Utils

type State =
    { PlayerPos: int array
      PlayerPoints: int64 array
      Die: int
      NextPlayer: int
      ThrowCount: int
      }


let progressDie d s =
    ((d + s - 1) % 100) + 1

let progressPos p sw =
    ((p + sw - 1) % 10) + 1

let rec runGameTillPoints (state: State) targetPoints =
    let throw = state.Die + (progressDie state.Die 1) + (progressDie state.Die 2)
    let newThrowCount = state.ThrowCount + 3
    let newDie = progressDie state.Die 3
    let newPos = progressPos state.PlayerPos.[state.NextPlayer] throw
    let newPoints = state.PlayerPoints.[state.NextPlayer] + (int64 newPos)
    if (newPoints >= targetPoints) then
        let otherPlayerPoints = state.PlayerPoints.[1 - state.NextPlayer]
        otherPlayerPoints * (int64 newThrowCount)
    else
        let newState = { state with Die=newDie; NextPlayer=1-state.NextPlayer; ThrowCount=newThrowCount }
        newState.PlayerPos.[state.NextPlayer] <- newPos
        newState.PlayerPoints.[state.NextPlayer] <- newPoints
        //printfn "-----------------\n%O\n--------------------" newState
        runGameTillPoints newState targetPoints

let part1WinnerAt1000 (data: int array) =
    let state =
        { PlayerPos = [| data.[0]; data.[1] |]
          PlayerPoints = [| 0L; 0L |]
          Die = 1
          NextPlayer = 0
          ThrowCount = 0 }
    printfn "Intial state:\n%O\n=====================" state
    let res = runGameTillPoints state 1000L
    printfn "Part 1, loser-points * throws: %d" res


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
    0 // return an integer exit code
