
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
    | Op (_, 0, ls) -> ls |> List.map (fun e -> evaluateExpression e) |> List.sum
    | Op (_, 1, ls) -> ls |> List.map (fun e -> evaluateExpression e) |> List.reduce (fun a b -> a*b)
    | Op (_, 2, ls) -> ls |> List.map (fun e -> evaluateExpression e) |> List.min
    | Op (_, 3, ls) -> ls |> List.map (fun e -> evaluateExpression e) |> List.max
    | Op (_, 5, ls) ->
        let lval::rval::_ = ls |> List.map (fun e -> evaluateExpression e)
        if (lval > rval) then (uint64 1) else (uint64 0)
    | Op (_, 6, ls) ->
        let lval::rval::_ = ls |> List.map (fun e -> evaluateExpression e)
        if (lval < rval) then (uint64 1) else (uint64 0)
    | Op (_, 7, ls) ->
        let lval::rval::_ = ls |> List.map (fun e -> evaluateExpression e)
        if (lval = rval) then (uint64 1) else (uint64 0)
