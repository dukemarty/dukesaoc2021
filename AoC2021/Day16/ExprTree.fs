module ExprTree

open System

type Version = int
type TypeId = int

type Expression =
    | Lit of Version * UInt64
    | Op of Version * TypeId * (Expression list)

let rec evaluateExpression expr =
    match expr with
    | Lit (_, v) -> v
    | Op (_, 0, ls) -> List.sumBy evaluateExpression ls
    | Op (_, 1, ls) -> ls |> List.map evaluateExpression |> List.reduce (*)
    | Op (_, 2, ls) ->
        ls
        |> List.map evaluateExpression
        |> List.min
    | Op (_, 3, ls) ->
        ls
        |> List.map evaluateExpression
        |> List.max
    | Op (_, 5, ls) ->
        let lval :: rval :: _ = ls |> List.map evaluateExpression

        if (lval > rval) then
            (uint64 1)
        else
            (uint64 0)
    | Op (_, 6, ls) ->
        let lval :: rval :: _ = ls |> List.map evaluateExpression

        if (lval < rval) then
            (uint64 1)
        else
            (uint64 0)
    | Op (_, 7, ls) ->
        let lval :: rval :: _ = ls |> List.map evaluateExpression

        if (lval = rval) then
            (uint64 1)
        else
            (uint64 0)
    | _ -> failwith "Unhandled expression!"
