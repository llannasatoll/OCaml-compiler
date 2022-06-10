(* α変換 *)

open Knormal

(* メイン *)

let rec g expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, expr3, expr4) ->
      IfEqual (name1, name2, g expr3, g expr4)
  | IfLess (name1, name2, expr3, expr4) ->
      IfLess (name1, name2, g expr3, g expr4)
  | Let ((name, t), expr1, expr2) -> 
    if Variable(name) = expr2 then g expr1
    else Let((name, t), g expr1, g expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
      LetRec ((name, t), params, g expr1, g expr2)
  | Application (name, name_list) -> expr

(* Eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr
