(* β変換から定数伝播までを変化がなくなるまで繰り返す *)
(* simple_optimize : Knormal.t -> Knormal.t *)
let rec simple_optimize kprogram0 =
  let kprogram = Beta.f kprogram0 in		(* β変換 *)
  let kprogram = Eta.f kprogram in		(* η変換 *)
  let kprogram = Assoc.f kprogram in		(* 結合性変換 *)
  let kprogram = Elim.f kprogram in		(* 不要変数除去 *)
  let kprogram = Constf.f kprogram in		(* 定数伝播 *)
  if kprogram = kprogram0 then kprogram
			  else simple_optimize kprogram

(* 各種最適化を n 回、行う *)
(* optimize : Knormal.t -> int -> Knormal.t *)
let rec optimize kprogram0 n =
  let kprogram = simple_optimize kprogram0 in
  if n > 0 then (* let kprogram = Expand.f kprogram in *) (* 関数展開 *)
		let kprogram = simple_optimize kprogram in
		optimize kprogram (n - 1)
	   else kprogram

(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  let kprogram = Knormal.f program in	(* k-正規形に変換し、*)
  let (kprogram, env) = Typing.f kprogram in	(* 型推論をし、*)
  let rec print list = match list with 
  | [] -> print_endline("")
  | (x, t) :: rest -> print_endline(x);
  match t with 
  | Type.Int -> print_endline("int");
  | Type.Float -> print_endline("float");
  | Type.Fun(_,_) -> print_endline("fun");
  | _ -> print_endline("tvar"); print rest; in
  (*Knormal.print kprogram;
  print_endline("------knormal---------");

  let (kprogram, new_env) = Alpha.f kprogram env in	(* α変換し、*)
  Knormal.print kprogram;
  print_endline("------alpha---------");
  let kprogram = optimize kprogram 0 in	(* 各種最適化を施し、*)
  Knormal.print kprogram;
  print_endline("------optimaize---------");
  let cprogram = Closure.f kprogram in	(* クロージャ変換を行い、*)
   Closure.print cprogram;
   print new_env;
   print_endline("------closure---------");
  let cprogram = Prealloc.f cprogram new_env in	(* レジスタ割り当て前処理を行い、*)
  Closure.print cprogram;

  print_endline("------prealloc---------");
  let cprogram = Alloc.f cprogram in	(* レジスタ割り当てを行い、*)
  Closure.print cprogram;
  let asm_code = Code.f cprogram in	(* コード生成を行い、*)
  print_string asm_code	*)	(* 表示する。*)

  let (kprogram, new_env) = Alpha.f kprogram env in	(* α変換し、*)
  let kprogram = optimize kprogram 0 in	(* 各種最適化を施し、*)
  let cprogram = Closure.f kprogram in	(* クロージャ変換を行い、*)
  let cprogram = Prealloc.f cprogram new_env in	(* レジスタ割り当て前処理を行い、*)
  let cprogram = Alloc.f cprogram in	(* レジスタ割り当てを行い、*)
  let asm_code = Code.f cprogram in	
  print_string asm_code

(* スタートアップ *)
let _ = go ()
