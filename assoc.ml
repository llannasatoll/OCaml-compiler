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
  | Let ((name, t), expr1, expr2) -> (match expr1 with
      | Let ((name_, t_), expr1_, expr2_) ->
        let rec letrec v e1 e2 = (match e1 with 
        | Let (v_, e1_, e2_) -> Let (v_, e1_, (letrec v e2_ e2))
        | _ -> Let (v, e1, g e2))
        in (letrec (name, t) expr1 expr2)
      | _ -> Let((name, t), g expr1, g expr2))
  | LetRec ((name, t), params, expr1, expr2) ->
      LetRec ((name, t), params, g expr1, g expr2)
  | Application (name, name_list) -> expr

(* Assoc.f : Knormal.t -> Knormal.t *)

let f expr = g expr
