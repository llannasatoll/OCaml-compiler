(* Closure.t: クロージャ変換後の抽象構文木の型 *)

type closure_t = Cls of (string * Type.t) * (string * Type.t) list

type t = Number of int
       | Real of float
       | Variable of string
       | Op of string * Operator.t * string
       | IfEqual of string * string * t * t
       | IfLess of string * string * t * t
       | Let of (string * Type.t) * t * t
       | LetClosure of (string * Type.t) * closure_t * t
       | AppC of string (* 変数名 *) * string list
       | AppD of string (* ラベル名 *) * string list

type def_t = FunDef of (string * Type.t) * (string * Type.t) list *
					   (string * Type.t) list * t

type prog_t = Program of def_t list * t

(* Closure.print: 抽象構文木をプリントする関数（デバッグ用）  *)

let type_on = ref true	(* 型推論を実装したら true にする *)

let indent i = String.make i ' '

let string_of_closure (Cls ((f, _), lst)) = match lst with
    [] -> "[" ^ f ^ "]"
  | (x, _) :: rest ->
	"[" ^ f ^ ", [" ^
	List.fold_left (fun str (y, _) -> str ^ "," ^ y)
		       x
		       rest
	^ "]]"

let rec string_of_expr expr i = match expr with
    Number (num) -> string_of_int num
  | Real (f) -> string_of_float f
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
	"(" ^ arg1
	    ^ (match op with
		  Operator.Plus -> "+"
		| Operator.Minus -> "-"
		| Operator.Times -> "*"
		| Operator.Divide -> "/"
		| Operator.Mod -> " mod "
		| Operator.PlusDot -> "+."
		| Operator.MinusDot -> "-."
		| Operator.TimesDot -> "*."
		| Operator.DivideDot -> "/.")
	    ^ arg2 ^ ")"
  | IfEqual (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "=" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | IfLess (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "<" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | Let ((name, t), arg1, arg2) ->
        (if !type_on
	 then "let (" ^ name ^ ":" ^ Type.to_string t ^ ")="
	 else "let " ^ name ^ "=")
	^ string_of_expr arg1 (i+5+String.length name)
	^ " in\n"
	^ indent i ^ string_of_expr arg2 i
  | LetClosure ((name, t), closure, arg) ->
        (if !type_on
	 then "let_closure (" ^ name ^ ":" ^ Type.to_string t ^ ")="
	 else "let_closure " ^ name ^ "=")
	^ string_of_closure closure
	^ " in\n"
	^ indent i ^ string_of_expr arg i
  | AppC (name, args) ->
	"[" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ "]"
  | AppD (name, args) ->
	"(" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ ")"

let rec string_of_def (FunDef ((f, _), fvs, params, body)) =
  match fvs with
      [] -> "let rec " ^ f ^ " " ^
	      List.fold_left (fun str (x, t) ->
		if !type_on
		then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
		else str ^ x ^ " ")
	      ""
	      params ^ "=\n  " ^
	    string_of_expr body 2 ^ "\n"
    | (y, t) :: rest ->
	    "let rec " ^
	    f ^ List.fold_left (fun str (y, t) ->
		  if !type_on
		  then str ^ ", " ^ y ^ " : " ^ Type.to_string t
		  else str ^ "," ^ y)
		(" [" ^ if !type_on then y ^ " : " ^ Type.to_string t else y)
		rest ^ "] " ^
		List.fold_left (fun str (x, t) ->
		  if !type_on
		  then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
		  else str ^ x ^ " ")
		""
		params ^ "=\n  " ^
	      string_of_expr body 2 ^ "\n"

let rec string_of_prog (Program (def_list, expr)) =
  "{\n"
   ^ List.fold_left (fun str def -> str ^ string_of_def def)
		    "" def_list
   ^ "}\n"
   ^ string_of_expr expr 0

let print prog =
  let str = string_of_prog prog
  in (print_string str;
      print_newline ())

let rec destroy_v list v = match list with
	| [] -> []
	| one :: rest -> if one = v then destroy_v rest v else (one :: destroy_v rest v)

let rec fv expr = match expr with
	| Number (num) -> []
	| Real (f) -> []
	| Variable (name) -> [name]
	| Op (name1, op, name2) -> [name1; name2]
	| IfEqual (name1, name2, expr3, expr4) -> name1 :: name2 :: (List.append (fv expr3) (fv expr4))
	| IfLess (name1, name2, expr3, expr4) -> name1 :: name2 :: (List.append (fv expr3) (fv expr4))
	| Let ((name, t), expr1, expr2) -> List.append (fv expr1) (destroy_v (fv expr2) name)
	| LetClosure ((name, t), closure, arg) -> (match closure with
		 Cls ((name_, t_), listy) -> 
		 let rec makelist lst = match lst with
		 | [] -> []
		 | (one, t1) :: rest -> one :: makelist rest in
		 destroy_v (List.append (makelist listy) (fv arg)) name)
	| AppC (name, args) -> name :: args
	| AppD (name, args) -> args


(* クロージャ変換のメイン *)

let def_list = ref []

let rec c_e expr lst = 
	let rec isin lst1 v0 = match lst1 with
	| [] -> false
	| one1 :: rest1 -> if one1 = v0 then true else isin rest1 v0 in
	let rec delist lst2 kesulist = match kesulist with 
	| [] -> lst2
	| one2 :: rest2 -> destroy_v (delist lst2 rest2) one2 in
	let rec pairpick oldlst = match oldlst with
	| [] -> []
	| (ex, t) :: rest3 -> ex :: pairpick rest3 in
	let rec pairmake newlst = match newlst with
	| [] -> []
	| one4 :: rest4 -> (one4, Type.gen_type()) :: pairmake rest4 in 
	match expr with
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (name1, op, name2) -> Op (name1, op, name2)
  | Knormal.IfEqual (name1, name2, expr3, expr4) -> IfEqual (name1, name2, (c_e expr3 lst), (c_e expr4 lst))
  | Knormal.IfLess (name1, name2, expr3, expr4) -> IfLess (name1, name2, (c_e expr3 lst), (c_e expr4 lst))
  | Knormal.Let ((name, t), expr1, expr2) -> Let ((name, t), (c_e expr1 lst), (c_e expr2 lst))
	(*| Knormal.LetRec ((name, t), args, arg1, arg2) -> 
		let c_1 = (c_e arg1 lst) in
		let c_2 = (c_e arg2 lst) in
		let fv_e1 = fv c_1 in
		let listy = pairmake (delist fv_e1 (name :: pairpick args)) in
		def_list := (FunDef ((name, t), listy, args, c_1)) :: !def_list;
		LetClosure ((name, t), Cls((name, t), listy), c_2)
	多少賢いクロージャー
	| Knormal.LetRec ((name, t), args, arg1, arg2) ->
		let e_1 = c_e arg1 (name :: lst) in
		let check = (delist (fv e_1) (pairpick args)) in
		(match check with 
		| [] -> let e_2 = c_e arg2 (name :: lst) in	
		let _ = def_list := (FunDef ((name, t), [], args, e_1)) :: !def_list in
		LetClosure ((name, t), Cls((name, t), []), e_2)
		| first :: rest -> 
		let e_1 = (c_e arg1 lst) in
		let e_2 = (c_e arg2 lst) in
		let fv_e1 = fv e_1 in
		let listy = pairmake (delist fv_e1 (name :: pairpick args)) in
		def_list := (FunDef ((name, t), listy, args, e_1)) :: !def_list;
		LetClosure ((name, t), Cls((name, t), listy), e_2))*)
	| Knormal.LetRec ((name, t), args, arg1, arg2) ->
		let e_1 = c_e arg1 (name :: lst) in
		let check = (delist (fv e_1) (pairpick args)) in
		(match check with 
		| [] -> let e_2 = c_e arg2 (name :: lst) in	
			let _ = def_list := (FunDef ((name, t), [], args, e_1)) :: !def_list in
			if isin (fv e_2) name then LetClosure ((name, t), Cls((name, t), []), e_2) else e_2
		| first :: rest -> 
			let e_1 = (c_e arg1 lst) in
			let e_2 = (c_e arg2 lst) in
			let fv_e1 = fv e_1 in
			let listy = pairmake (delist fv_e1 (name :: pairpick args)) in
			def_list := (FunDef ((name, t), listy, args, e_1)) :: !def_list;
			LetClosure ((name, t), Cls((name, t), listy), e_2))
  | Knormal.Application (name, name_list) ->
		try let _ = String.index name '_' in 
		if isin lst name then AppD(name, name_list) else AppC(name, name_list)
		with 
		| Not_found -> AppD(name, name_list)

let c_p program = let e_ = c_e program [] in Program (List.rev !def_list, e_ )

(* Closure.f: クロージャ変換の入口 *)
(* Closure.f : Knormal.t -> Closure.prog_t *)
let f program = c_p program