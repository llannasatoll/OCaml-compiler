(* β変換 *)

open Knormal

exception TypeError

let rec v_check env v = (try Env.get env v with
  Env.UnboundVariable v -> Variable(v))

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> v_check env name
  | Op (name1, op, name2) -> (match v_check env name1, v_check env name2 with
    | Number(x1), Number(x2) -> (match op with
          Operator.Plus -> Number(x1 + x2)
        | Operator.Minus -> Number(x1 - x2)
        | Operator.Times -> Number(x1 * x2)
        | Operator.Divide -> Number(x1 / x2)
        | Operator.Mod -> Number(x1 mod x2)
        | _ -> raise TypeError)
    | Real(f1), Real(f2) -> (match op with
          Operator.PlusDot-> Real(f1 +. f2)
        | Operator.MinusDot -> Real(f1 -. f2)
        | Operator.TimesDot -> Real(f1 *. f2)
        | Operator.DivideDot -> Real(f1 /. f2)
        | _ -> raise TypeError)
    |  _ -> expr)
  | IfEqual (name1, name2, expr3, expr4) -> (match v_check env name1, v_check env name2 with
    | Number(x1), Number(x2) -> if x1 = x2 then g expr3 env else g expr4 env
    | Real(x1), Real(x2) -> if x1 = x2 then g expr3 env else g expr4 env
    | _, _ -> IfEqual (name1, name2, g expr3 env, g expr4 env))
  | IfLess (name1, name2, expr3, expr4) -> (match v_check env name1, v_check env name2 with
    | Number(x1), Number(x2) -> if x1 < x2 then g expr3 env else g expr4 env
    | Real(x1), Real(x2) -> if x1 < x2 then g expr3 env else g expr4 env
    | _, _ -> IfEqual (name1, name2, g expr3 env, g expr4 env))
  | Let ((name, t), expr1, expr2) -> (match expr1 with
    | Number(v) -> Let ((name, t), expr1, g expr2 (Env.add env name expr1))
    | Real(v) -> Let ((name, t), expr1, g expr2 (Env.add env name expr1))
    | _ -> Let((name, t), g expr1 env, g expr2 env))
  | LetRec ((name, t), params, expr1, expr2) -> LetRec ((name, t), params, g expr1 env, g expr2 env)
  | Application (name, name_list) -> expr
  

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
