open Knormal

(* 型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t

(* 型エラー *)
exception Error of Knormal.t * Type.t * Type.t

let t_env = ref []

(* 型変数を中身で置き換える。返ってくる型には型変数は含まれない *)
(* deref_type : Type.t -> Type.t *)
let rec deref_type t = match t with
    Type.Int -> Type.Int
  | Type.Float -> Type.Float
  | Type.Fun (ts, t') -> Type.Fun(List.map deref_type ts, deref_type t')
  | Type.TVar (r) -> match !r with
			None -> r := Some (Type.Int);
				     Type.Int
		      | Some (t') -> let t'' = deref_type t' in
				     r := Some (t'');
				     t''

(* (変数, 型) の型を deref する *)
(* deref_id_type : (string * Type.t) -> (string * Type.t) *)
let rec deref_id_type (x, t) = (x, deref_type t)

(* 項にでてくる型を deref する *)
(* deref_term : Knormal.t -> Knormal.t *)
let rec deref_term expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (name)
  | Op (arg1, op, arg2) -> expr
  | IfEqual (v1, v2, e3, e4) ->
      IfEqual (v1, v2, deref_term e3, deref_term e4)
  | IfLess (v1, v2, e3, e4) ->
      IfLess (v1, v2, deref_term e3, deref_term e4)
  | Let (xt, e1, e2) -> Let (deref_id_type xt, deref_term e1, deref_term e2)
  | LetRec (xt, params, e1, e2) ->
      LetRec (deref_id_type xt, List.map deref_id_type params,
	      deref_term e1, deref_term e2)
  | Application (e, es) -> expr

(* r が型 t に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r t = match t with
    Type.Int -> false
  | Type.Float -> false
  | Type.Fun (ts, t') -> List.exists (occur r) ts || occur r t'
  | Type.TVar (r') ->
      if r == r' then true else
	match !r' with
	    None -> false
	  | Some (t') -> occur r t'

(* t1 = t2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify t1 t2 = match (t1, t2) with
    (Type.Int, Type.Int) -> ()
  | (Type.Float, Type.Float) -> ()
  | (Type.Fun (t1s, t1'), Type.Fun (t2s, t2')) ->
      (try List.iter2 unify t1s t2s with
         Invalid_argument _ -> raise (Unify (t1, t2)));
      unify t1' t2'
  | (Type.TVar (r1), Type.TVar (r2)) when r1 == r2 -> ()
  | (Type.TVar ({ contents = Some(t1') }), _) -> unify t1' t2
  | (_, Type.TVar ({ contents = Some(t2') })) -> unify t1 t2'
  | (Type.TVar ({ contents = None } as r1), _) -> (* t2 は TVar とは限らない *)
      if occur r1 t2 then raise (Unify (t1, t2))
		     else r1 := Some (t2)
  | (_, Type.TVar ({ contents = None } as r2)) -> (* t1 は TVar とは限らない *)
      if occur r2 t1 then raise (Unify (t1, t2))
		     else r2 := Some (t1)
  | (_, _) -> raise (Unify (t1, t2))

(*渡された型変数のタイプのみを返す*)
let rec pick_type x = match x with
 (y, t) -> t

(*環境envにlist(x1:t1, x2:t2,...)を追加して、新しいenvを返す*)
let rec env_addlist list env = match list with 
  | [] -> env
  | (x, t) :: rest -> t_env := (x, t) :: !t_env;
  env_addlist rest (Env.add env x t)
 
(* 型推論 *)
(* g : Knormal.t -> (string * Type.t) list -> Type.t *)
let rec g expr env = try (match expr with
   Number (num) -> Type.Int
  | Real (f) -> Type.Float
  | Variable (name) -> Env.get env name
  | Op (arg1, op, arg2) -> (match op with
		  Operator.Plus | Operator.Minus | Operator.Times | Operator.Divide | Operator.Mod -> 
        unify (Env.get env arg1) Type.Int;
        unify Type.Int (Env.get env arg2); 
        Type.Int
		| Operator.PlusDot | Operator.MinusDot | Operator.TimesDot | Operator.DivideDot -> 
      unify (Env.get env arg1) Type.Float;
      unify Type.Float (Env.get env arg2); 
      Type.Float)
  | IfEqual (v1, v2, e3, e4) -> 
    let t3 = g e3 env in 
    let t4 = g e4 env in
    unify (Env.get env v1) (Env.get env v2); 
    unify t3 t4; 
    t3
  | IfLess (v1, v2, e3, e4) ->
    let t3 = g e3 env in 
    let t4 = g e4 env in
    unify (Env.get env v1) (Env.get env v2); 
    unify t3 t4; 
    t3
  | Let ((name, t), arg1, arg2) ->
    let t1 = g arg1 env in
    unify t t1;
    let t2 = g arg2 (Env.add env name t) in
    t_env := (name, t) :: !t_env;
    t2
  | LetRec ((name, t), params, e1, e2) -> 
    let new_env1 = Env.add env name t in  
    t_env := (name, t) :: !t_env;
    unify t (Type.Fun((List.map pick_type params), g e1 (env_addlist params new_env1)));
    g e2 new_env1
  | Application (e, es) -> 
    let t = Type.gen_type() in
    let rec get_env str = Env.get env str in
    unify (Env.get env e) (Type.Fun((List.map get_env es), t));
    t_env := (e, (Env.get env e)) :: !t_env;
    t)
  with Unify (t1, t2) ->
    Knormal.print (deref_term expr);
    Type.print (deref_type t1);
    Type.print (deref_type t2);
    raise (Error (deref_term expr, deref_type t1, deref_type t2))

(* 型推論の入り口 *)
(* Typing.f : Knormal.t -> Knormal.t *)
let f expr =
  (try unify Type.Int (g expr Env.empty_env) with
   Unify _ -> failwith "top level does not have type int");
  (deref_term expr, !t_env)
