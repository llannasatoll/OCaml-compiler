(* レジスタ割り当て前処理 *)

open Register

exception ListError
exception TypeError
exception TypeError2

(*letの1からnまでの入れ子を作る。lastが最後に実行したい文*)
let rec makelet vlst rlst last = match vlst, rlst with
  | [], [] -> last
  | (v_first,v_t) :: v_rest, (r_first, t_r) :: r_rest -> Closure.Let((v_first, v_t), Closure.Variable(r_first), makelet v_rest r_rest last)
  | _, _ -> raise ListError

(*letの1からnまでの入れ子を作る。lastが最後に実行したい文のタイプなし版*)
let rec makelet2 vlst rlst last = match vlst, rlst with
  | [], [] -> last
  | v_first :: v_rest, r_first :: r_rest -> if Register.is_fregister v_first then Closure.Let((v_first, Type.Float), Closure.Variable(r_first), makelet2 v_rest r_rest last)
                                            else Closure.Let((v_first, Type.Int), Closure.Variable(r_first), makelet2 v_rest r_rest last)
  | _, _ -> raise ListError

(*lstのなかにv0が入っているかチェック*)
let rec isin lst_ v0 = match lst_ with
	| [] -> false
	| one :: rest -> if one = v0 then true else isin rest v0

(*与えられたリストの変数の、型にあったレジスタを作って返す*)
let rec make_fr_reg list env fi rj = match list with 
| [] -> []
| first :: rest -> if Typing.deref_type (Env.get env first) = Type.Float then (make_fregister fi) :: (make_fr_reg rest env (fi+1) rj)
                  else if Typing.deref_type (Env.get env first) = Type.Int then (make_register rj) :: (make_fr_reg rest env fi (rj+1))
                  else raise TypeError

let rec make_fr_reg2 list env fi rj = match list with 
| [] -> []
| (v, t) :: rest -> (match Typing.deref_type t with
                  | Type.Int -> ((make_register rj), Type.Int) :: (make_fr_reg2 rest env fi (rj+1))
                  | Type.Float -> ((make_fregister fi),Type.Float) :: (make_fr_reg2 rest env (fi+1) rj)
                  | _ -> raise TypeError)

(* メイン *)
let rec p_e expr env = match expr with
    Closure.Number (num) -> Closure.Number (num)
  | Closure.Real (f) -> Closure.Real (f)
  | Closure.Variable (name) -> Closure.Variable (name)
  | Closure.Op (name1, op, name2) -> Closure.Op (name1, op, name2)
  | Closure.IfEqual (name1, name2, expr3, expr4) -> Closure.IfEqual (name1, name2, (p_e expr3 env), (p_e expr4 env))
  | Closure.IfLess (name1, name2, expr3, expr4) -> Closure.IfLess (name1, name2, (p_e expr3 env), (p_e expr4 env))
  | Closure.Let ((name, t), expr1, expr2) -> Closure.Let ((name, t), (p_e expr1 env), (p_e expr2 env))
  | Closure.LetClosure ((name, t), closure, expr) -> Closure.LetClosure ((name, t), closure, p_e expr env)
  | Closure.AppC (name, args) -> 
    let r_lst = make_fr_reg args env 0 1 in
   (* (match Typing.deref_type (Env.get env name) with
    | Type.Int -> *)let registercls = Closure.AppC ("_R_0", r_lst) in
        Closure.Let(("_R_0", Type.Int), Closure.Variable(name), makelet2 r_lst args registercls)
   (* | Type.Float -> let registercls = Closure.AppC ("_F_0", r_lst) in
        Closure.Let(("_F_0", Type.Float), Closure.Variable(name), makelet2 r_lst args registercls)
    | Type.Fun(list, t) -> (match Typing.deref_type t with
                        | Type.Int -> let registercls = Closure.AppC ("_R_0", r_lst) in
                           Closure.Let(("_R_0", Type.Int), Closure.Variable(name), makelet2 r_lst args registercls)
                        | Type.Float -> let registercls = Closure.AppC ("_F_0", r_lst) in
                          Closure.Let(("_F_0", Type.Float), Closure.Variable(name), makelet2 r_lst args registercls)
                        | _ -> raise TypeError)
    | _ -> raise TypeError)*)
  | Closure.AppD (name, args) -> 
    let r_lst = make_fr_reg args env 0 1 in
    let registercls = Closure.AppD (name, r_lst) in
    makelet2 r_lst args registercls

let f_def def env =  
  match def with 
 | Closure.FunDef ((name, t), y_lst, v_lst, expr) ->
  let r_lst = make_fr_reg2 v_lst env 0 1 in
  if isin (Closure.fv expr) name 
  then (*(match Typing.deref_type (Env.get env name) with
    | Type.Int -> *)Closure.FunDef ((name, t), y_lst, r_lst, Closure.Let((name, t), Closure.Variable("_R_0"), makelet v_lst r_lst (p_e expr env)))
   (* | Type.Float -> (Closure.FunDef ((name, t), y_lst, r_lst, Closure.Let((name, t), Closure.Variable("_F_0"), makelet v_lst r_lst (p_e expr env))))
    | Type.Fun(list, t) -> (match Typing.deref_type t with
                        | Type.Int -> (Closure.FunDef ((name, t), y_lst, r_lst, Closure.Let((name, t), Closure.Variable("_R_0"), makelet v_lst r_lst (p_e expr env))))
                        | Type.Float -> (Closure.FunDef ((name, t), y_lst, r_lst, Closure.Let((name, t), Closure.Variable("_F_0"), makelet v_lst r_lst (p_e expr env))))
                        | _ -> raise TypeError)
    | _ -> raise TypeError)*)
  else (Closure.FunDef ((name, t), y_lst, r_lst, makelet v_lst r_lst (p_e expr env)))

let f_program (Closure.Program (def_list, expr)) env =
  let rec prolist list = match list with 
  | [] -> []
  | d :: rest -> (f_def d env) :: prolist rest in 
  Closure.Program (prolist def_list, p_e expr env)

(* Prealloc.f : Closure.prog_t -> Closure.prog_t *)
let f program env = f_program program env 
