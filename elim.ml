(* α変換 *)

open Knormal


let rec check x list = match list with
  | [] -> false
  | first :: rest -> if first = x then true else (check x rest)

let rec check2 x list = match list with
  | [] -> true
  | (first, t) :: rest -> if first = x then false else (check2 x rest)

let rec u expr x = match expr with 
    Number (num) -> false
  | Real (f) -> false
  | Variable (name) -> (name = x)
  | Op (name1, op, name2) -> (name1 = x) || (name2 = x)
  | IfEqual (name1, name2, expr3, expr4) -> (name1 = x) || (name2 = x) || (u expr3 x) || (u expr4 x)
  | IfLess (name1, name2, expr3, expr4) -> (name1 = x) || (name2 = x) || (u expr3 x) || (u expr4 x)
  | Let ((name, t), expr1, expr2) -> (u expr1 x) || ((x <> name) && (u expr2 x))
  | LetRec ((name, t), params, expr1, expr2) -> ((name <> x) && (check2 x params) && (u expr1 x)) || ((x <> name) && (u expr2 x))
  | Application (name, name_list) -> (name = x) || check x name_list

let rec te expr = match expr with
    Number (num) -> true
  | Real (f) -> true
  | Variable (name) -> true
  | Op (name1, op, name2) -> true
  | IfEqual (name1, name2, expr3, expr4) -> false
  | IfLess (name1, name2, expr3, expr4) -> false
  | Let ((name, t), expr1, expr2) -> false
  | LetRec ((name, t), params, expr1, expr2) -> false
  | Application (name, name_list) -> false


  
(* メイン *)
let rec g expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, expr3, expr4) -> IfEqual (name1, name2, g expr3, g expr4)
  | IfLess (name1, name2, expr3, expr4) -> IfLess (name1, name2, g expr3, g expr4)
  | Let ((name, t), expr1, expr2) -> 
    let e1 = g expr1 in
    let e2 = g expr2 in
    if (te e1 && (u e2 name = false)) then e2 else Let ((name, t), e1, e2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let e1 = g expr1 in
    let e2 = g expr2 in
    if u e2 name = false then e2 else LetRec ((name, t), params, e1, e2)
  | Application (name, name_list) -> expr

(* Assoc.f : Knormal.t -> Knormal.t *)

let f expr = g expr
