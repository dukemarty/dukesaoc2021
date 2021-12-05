// Learn more about F# at http://fsharp.org

open System

let draws = DataInput.draws
let cardsRaw = DataInput.cards

let applyNumberToCard number card =
    let tickOff i = if (i = number) then (-1) else i
    card |> List.map (fun r -> r |> List.map tickOff)

let checkCardForFinished card =
    let checkLine r = (r |> List.sum) = -5

    let cols =
        [ 0; 1; 2; 3; 4 ]
        |> Seq.map (fun i -> (card |> List.map (fun (r: int list) -> r.[i])))
        |> List.ofSeq

    let rowsFull =
        List.reduce (fun a b -> a || b) (card |> List.map checkLine)

    let colsFull =
        List.reduce (fun a b -> a || b) (cols |> List.map checkLine)

    rowsFull || colsFull


let calcCardPoints (card: int list list) =
    card
    |> List.map
        (fun l ->
            l
            |> List.filter (fun n -> not (n = -1))
            |> List.sum)
    |> List.sum

let part1BingoPoints =
    let mutable i = 0
    let mutable finishedCardIndex = -1
    let mutable finished = false
    let mutable cards = cardsRaw |> List.indexed

    while (i < draws.Length && not finished) do
        cards <-
            cards
            |> List.map (fun (_, c) -> applyNumberToCard draws.[i] c)
            |> List.indexed

        //printfn "Updated cards after %d: %A" draws.[i] cards
        //printfn "------------------------------------"

        let finishedCards =
            cards
            |> List.map (fun (i, c) -> (i, (checkCardForFinished c)))
            |> List.filter (fun (i, b) -> b)

        if (finishedCards.Length > 0) then
            let (x, _) = List.head (finishedCards)
            finishedCardIndex <- x
            finished <- true
        else
            i <- i + 1

    printfn "Finished while-loop, i=%d, fCI=%d, f=%b" i finishedCardIndex finished

    let (_, finishedCard) = cards.[finishedCardIndex]
    let remainingCardPoints = calcCardPoints finishedCard
    let lastNumber = draws.[i]
    printfn "Resulting points: %d (%d * %d)" (remainingCardPoints * lastNumber) remainingCardPoints lastNumber

[<EntryPoint>]
let main argv =
    printfn "Day 4: Giant Squid\n==================\n"
    //printfn "%A" draws
    //printfn "%A" cardsRaw
    part1BingoPoints
    0 // return an integer exit code
