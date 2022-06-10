(* β変換 *)

open Knormal

let rec v_check env v = (try Env.get env v with
  Env.UnboundVariable v -> v)

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable(v_check env name)
  | Op (name1, op, name2) -> Op (v_check env name1, op, v_check env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
      IfEqual (v_check env name1, v_check env name2,
         g expr3 env,
         g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
      IfLess (v_check env name1, v_check env name2,
        g expr3 env,
        g expr4 env)
  | Let ((name, t), expr1, expr2) -> (match expr1 with
        | Variable(v) ->
        let new_expr1 = v_check env v in
        Let ((name, t), Variable(new_expr1), g expr2 (Env.add env name new_expr1))
        | _ -> Let((name, t), g expr1 env, g expr2 env))
    | LetRec ((name, t), params, expr1, expr2) ->
        LetRec ((name, t), params, g expr1 env, g expr2 env)
    | Application (name, name_list) ->
        Application (v_check env name,
              List.map (fun var -> v_check env var) name_list)
  

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
