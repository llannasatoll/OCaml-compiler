(* Intel 用コード生成 *)

open Closure
open Register

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_hp"
let r_hp = "_R_hp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"
let f_tmp = "_F_tmp"

(* instructions *)

let label l = l ^ ":\n"
let movqi i r2    = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movq r1 r2    = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let movq2 i r1 r2 = "	movq " ^ string_of_int i ^ "(" ^ r1 ^ "), " ^ r2 ^"\n"
let movq3 r1 i r2 = "	movq " ^ r1 ^ ", " ^ string_of_int i ^ "(" ^ r2 ^ ")\n"
let leaq f r    = "	leaq " ^ f ^ "(%rip), " ^ r ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addqi i r2  = "	addq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let cqto =	"	cqto\n"
let idivq r =	"	idivq " ^ r ^ "\n"
let cmpq r1 r2 =
  if is_fregister r1
  then		"	ucomisd " ^ r1 ^ ", " ^ r2 ^ "\n"
  else		"	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let pushq r =
  if is_fregister r
  then			subqi 8 r_sp ^
		"	movsd " ^ r ^ ", 0(" ^ r_sp ^ ")\n"
  else		"	pushq " ^ r ^ "\n"
let popq r  =	
  if is_fregister r
  then		"	movsd 0(" ^ r_sp ^ "), " ^ r ^ "\n" ^
			addqi 8 r_sp
  else		"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let calls r =	"	call *" ^ r ^ "\n"
let ret =	"	ret\n"

let movsd f1 f2  = "	movsd " ^ f1 ^ ", " ^ f2 ^ "\n"
let addsd f1 f2  = "	addsd " ^ f1 ^ ", " ^ f2 ^ "\n"
let subsd f1 f2  = "	subsd " ^ f1 ^ ", " ^ f2 ^ "\n"
let mulsd f1 f2  = "	mulsd " ^ f1 ^ ", " ^ f2 ^ "\n"
let divsd f1 f2  = "	divsd " ^ f1 ^ ", " ^ f2 ^ "\n"

(* headers *)

let float_list = ref []
let get_float_label f =
  try
    List.assoc f !float_list
  with Not_found ->
	 let label = Gensym.f "f" in
	 float_list := (f, label) :: !float_list;
	 label

let movsdi double f =
  let label = get_float_label double in
  "	movsd " ^ label ^ "(%rip), " ^ f ^ "\n"

let float_data () =
		"\n	.literal8\n" ^
		List.fold_left (fun str (f, label) ->
			str ^
			"	.align 3\n" ^
			label ^ ": # " ^ string_of_float f ^ "\n" ^
			"	.long " ^ Int32.to_string (gethi f) ^ "\n" ^
			"	.long " ^ Int32.to_string (getlo f) ^ "\n")
		  ""
		  !float_list

let top =	"	.text\n"
let middle =	"	\n" ^
		"	.globl _asm_main\n" ^
		"_asm_main: # main entry point\n" ^
			pushq "%rbx" ^
			pushq "%r12" ^
			pushq "%r13" ^
			pushq "%r14" ^
			pushq "%r15" ^
			pushq r_bp ^
			movq (make_register 3) r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
			popq r_bp ^
			popq "%r15" ^
			popq "%r14" ^
			popq "%r13" ^
			popq "%r12" ^
			popq "%rbx" ^
			ret

(* push/pop registers *)

let rec push_live live = match live with
    [] -> ""
  | var :: rest -> pushq var ^ push_live rest

let rec pop_live live = match live with
    [] -> ""
  | var :: rest -> pop_live rest ^ popq var

(* load/store free variables *)

let rec load_fv fv_list i r0 = match fv_list with
    [] -> ""
  | (r, t) :: rest -> movq2 i r0 r ^ load_fv rest (i+8) r0
	(*
	else movsd ((string_of_int i)^"(_R_0)") r ^ load_fv rest (i+8) r0
*)
let rec store_fv fv_list i r0 = match fv_list with
    [] -> ""
  | (r, t) :: rest ->
        movq3 r i r0 ^
	store_fv rest (i+8) r0

let rec count lst = match lst with
  | [] -> 0
  | first :: rest -> 1 + count rest


(* メイン *)
let rec s_e expr z live = match expr with
	Closure.Number (num) -> movq ("$"^(string_of_int num)) z
| Closure.Real (f) -> movsdi f z
| Closure.Variable (name) -> if name = z then "" else if is_fregister name then movsd name z else movq name z
| Closure.Op (name1, op, name2) -> (match op with
	|	Operator.Plus -> 
		let l1 = (movq name1 r_ax) in
		let l2 = (addq name2 r_ax) in
		let l3 = (movq r_ax z) in
		(l1^l2^l3)
	| Operator.Minus ->
		let l1 = (movq name1 r_ax) in
		let l2 = (subq name2 r_ax) in
		let l3 = (movq r_ax z) in
		(l1^l2^l3)
	| Operator.Times -> 
		let l1 = (movq name1 r_ax) in
		let l2 = (imulq name2 r_ax) in
		let l3 = (movq r_ax z) in
		(l1^l2^l3)
	| Operator.Divide ->
		let l1 = (movq name1 r_ax) in
		let l2 = cqto in
		let l3 = idivq name2 in
		let l4 = movq r_ax z in
		(l1^l2^l3^l4)
	| Operator.Mod ->
		let l1 = (movq name1 r_ax) in
		let l2 = cqto in
		let l3 = idivq name2 in
		let l4 = movq r_dx z in
		(l1^l2^l3^l4)
	| Operator.PlusDot -> 	
		let l1 = (movsd name1 f_tmp) in
		let l2 = (addsd name2 f_tmp) in
		let l3 = (movsd f_tmp z) in
		(l1^l2^l3)
	| Operator.MinusDot ->
		let l1 = (movsd name1 f_tmp) in
		let l2 = (subsd name2 f_tmp) in
		let l3 = (movsd f_tmp z) in
		(l1^l2^l3)
	| Operator.TimesDot -> 
		let l1 = (movsd name1 f_tmp) in
		let l2 = (mulsd name2 f_tmp) in
		let l3 = (movsd f_tmp z) in
		(l1^l2^l3)
	| Operator.DivideDot ->
		let l1 = (movsd name1 f_tmp) in
		let l2 = (divsd name2 f_tmp) in
		let l3 = (movsd f_tmp z) in
		(l1^l2^l3))
| Closure.IfEqual (name1, name2, expr3, expr4) -> 
	let f1 = Gensym.f "l" in
	let f2 = Gensym.f "l" in
	let l1 = cmpq name1 name2 in 
	let l2 = jne f1 in
	let l3 = s_e expr3 z live in
	let l4 = jmp f2 in
	let l5 = label f1 in
	let l6 = s_e expr4 z live in
	let l7 = label f2 in
	l1^l2^l3^l4^l5^l6^l7
| Closure.IfLess (name1, name2, expr3, expr4) -> 
	let f1 = Gensym.f "l" in
	let f2 = Gensym.f "l" in
	let l1 = cmpq name1 name2 in 
	let l2 = jle f1 in
	let l3 = s_e expr3 z live in
	let l4 = jmp f2 in
	let l5 = label f1 in
	let l6 = s_e expr4 z live in
	let l7 = label f2 in
	l1^l2^l3^l4^l5^l6^l7
| Closure.Let ((name, t), expr1, expr2) -> 
	let l1 = s_e expr1 name (List.append live (destroy_v (fv expr2) name)) in
	let l2 = s_e expr2 z live in
	l1^l2
| Closure.LetClosure ((name, t), Closure.Cls((f_, t_), ylst), expr) -> 
	let l = count ylst in
	let l1 = movq r_hp name in
	let l2 = addqi (8*(l+1)) r_hp in
	let l3 = leaq f_ r_dx in
	let l4 = movq3 r_dx 0 name in
	let l5 = store_fv ylst 8 name in
	let l6 = s_e expr z live in
	l1^l2^l3^l4^l5^l6
| Closure.AppC (name, args) -> 
	let l1 = push_live live in
	let l2 = movq2 0 "_R_0" r_dx in
	let l3 = calls r_dx in
	let l4 = (if z = "_R_0" then "" else movq "_R_0" z) in
	let l5 = pop_live live in
	l1^l2^l3^l4^l5
| Closure.AppD (name, args) -> 
	let l1 = (push_live live) in
	let l2 = (call name) in
	if is_fregister z
	then 
	let l3 = (if (z = "_F_0") then "" else (movsd f_tmp z)) in
	let l4 = (pop_live live) in
	l1^l2^l3^l4
	else 
	let l3 = (if (z = "_R_0") then "" else (movq "_R_0" z)) in
	let l4 = (pop_live live) in
	l1^l2^l3^l4

let rec s_d (Closure.FunDef ((name, t), lst1, lst2, expr)) = 
	let l1 = label name in
	let l2 = load_fv lst1 8 (make_register 0) in
	if (Typing.deref_type t) = Type.Int then
		let l3 = s_e expr (make_register 0) [] in
		let l4 = ret in
		l1^l2^l3^l4
	else
	let l3 = s_e expr (make_fregister 0) [] in
	let l4 = ret in
	l1^l2^l3^l4
	
	
let rec f_program (Closure.Program (def_list, expr)) = 
	let l1 = top in 
	let rec l2 list = match list with
		| [] -> ""
		| one :: rest -> (s_d one)^(l2 rest) in
	let l3 = middle in
	(*
	let num = retcount expr in
	let l4 = (if num = 0 then "" else subqi (num*8) r_sp) in*)
	let l5 = s_e expr (make_register 0) [] in
	let l6 = last in
	l1^(l2 def_list)^l3^l5^l6
	
	
(* Code.f : Closure.prog_t -> string *)

let f program =
  let str = f_program program in
  str ^ float_data ()
