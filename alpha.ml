(* α変換 *)

open Knormal

let new_t_env = ref [("", Type.gen_type())]

(* params に新しい名前をつけ、それを環境に追加する *)
let rec add_params env params t_env = match params with
    [] -> env
  | (first, t) :: rest -> let newname = (Gensym.f first) in 
		new_t_env := (newname, Env.get t_env first) :: !new_t_env;
	add_params (Env.add env first newname) rest t_env


(* メイン *)

let rec g expr env t_env =
	match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> (try Variable (Env.get env name) with
		| Env.UnboundVariable(name1) -> Variable(name1))
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (Env.get env name1, Env.get env name2,
		 g expr3 env t_env,
		 g expr4 env t_env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (Env.get env name1, Env.get env name2,
		g expr3 env t_env,
		g expr4 env t_env)
  | Let ((name, t), expr1, expr2) ->
	let new_name = Gensym.f name in
	new_t_env := (new_name, Env.get t_env name) :: !new_t_env;
	let new_expr1 = g expr1 env t_env in
	let new_env = Env.add env name new_name in
	let new_expr2 = g expr2 new_env t_env in
	Let ((new_name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
	let new_name = Gensym.f name in
	new_t_env := (new_name, Env.get t_env name) :: !new_t_env;
	let new_env = add_params env params t_env in
	let new_params = List.map (fun (param, t) -> (Env.get new_env param, t))
				  params in
	let new_expr1 = g expr1 (Env.add new_env name new_name) t_env in
	let new_expr2 = g expr2 (Env.add env name new_name) t_env in
	LetRec ((new_name, t), new_params, new_expr1, new_expr2)
  | Application (name, name_list) -> Application (Env.get env name,
		     List.map (fun var -> Env.get env var) name_list)
(* Alpha.f : Knormal.t -> Knormal.t *)

let pro expr env = g expr Env.empty_env env

let f expr env = let tmp = (pro expr env) in
	(tmp, !new_t_env)
