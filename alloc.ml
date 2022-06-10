open Register

exception NoRegister
exception ListError
exception TypeError

let rec count lst rn fn = match lst with
  | [] -> (rn, fn)
  | (first, t) :: rest -> if Typing.deref_type t = Type.Int then count rest (rn+1) fn 
                    else count rest rn (fn+1)

let rec makeclosure y_lst env = match y_lst with
  | [] -> []
  | (first, t) :: rest -> (Env.get env first, t) :: makeclosure rest env

let rec make_registerlst list rnum fnum = match list with 
| [] -> []
| (y, t) :: rest -> if Typing.deref_type t = Type.Int then ((Register.make_register rnum), Type.Int) :: (make_registerlst rest (rnum+1) fnum)
                    else if Typing.deref_type t = Type.Float then ((Register.make_fregister fnum), Type.Float) :: (make_registerlst rest rnum (fnum+1))
                    else raise TypeError

let rcounter = ref 12
let fcounter = ref 15

let rec a_e expr env = try
match expr with
    Closure.Number (num) -> Closure.Number (num)
  | Closure.Real (f) -> Closure.Real (f)
  | Closure.Variable (name) -> 
    if (Register.is_register name) then let _ = Env.add env name name in Closure.Variable(name)
    else Closure.Variable (Env.get env name)
  | Closure.Op (name1, op, name2) -> Closure.Op (Env.get env name1, op, Env.get env name2)
  | Closure.IfEqual (name1, name2, expr3, expr4) -> let tmp = (a_e expr3 env) in Closure.IfEqual (Env.get env name1, Env.get env name2, tmp, (a_e expr4 env))
  | Closure.IfLess (name1, name2, expr3, expr4) -> let tmp = (a_e expr3 env) in Closure.IfLess (Env.get env name1, Env.get env name2, tmp, (a_e expr4 env))
  | Closure.Let ((name, t), expr1, expr2) ->
    if (Register.is_register name) then let tmp = a_e expr1 env in Closure.Let ((name, t), tmp, a_e expr2 env)
    else   (match Typing.deref_type t with
    | Type.Int -> (if !rcounter < 2 then raise NoRegister else (rcounter := !rcounter - 1;
        let r = Register.make_register !rcounter in 
        let tmp = a_e expr1 env in (Closure.Let ((r, t), tmp, a_e expr2 (Env.add env name r)))))
    | Type.Float -> if !fcounter < 1 then raise NoRegister else (fcounter := !fcounter - 1;
        let r = Register.make_fregister !fcounter in 
        let tmp = a_e expr1 env in (Closure.Let ((r, t), tmp, a_e expr2 (Env.add env name r))))
    (*| Type.Fun(list, t_) -> (match Typing.deref_type t_ with
                        | Type.Int -> (if !rcounter < 2 then raise NoRegister else (rcounter := !rcounter - 1;
                            let r = Register.make_register !rcounter in 
                            let tmp = a_e expr1 env in (Closure.Let ((r, t_), tmp, a_e expr2 (Env.add env name r)))))
                        | Type.Float -> if !fcounter < 1 then raise NoRegister else (fcounter := !fcounter - 1;
                            let r = Register.make_fregister !fcounter in 
                            let tmp = a_e expr1 env in (Closure.Let ((r, t_), tmp, a_e expr2 (Env.add env name r))))
                        | _ -> raise TypeError)*)
    | _ -> raise TypeError)
  | Closure.LetClosure ((name, t), Closure.Cls((f_, t_), ylst), expr) -> 
  (*(match Typing.deref_type t with
    | Type.Int -> *)(if !rcounter < 2 then raise NoRegister else rcounter := !rcounter - 1;
        let r = Register.make_register !rcounter in 
        let new_env = Env.add env name r in
        let closure = Closure.Cls((f_, t_), makeclosure ylst new_env) in
        Closure.LetClosure ((r, t), closure, a_e expr new_env))
    (*| Type.Float -> (if !fcounter < 1 then raise NoRegister else fcounter := !fcounter - 1;
        let r = Register.make_fregister !fcounter in 
        let new_env = Env.add env name r in
        let closure = Closure.Cls((f_, t_), makeclosure ylst new_env) in
        Closure.LetClosure ((r, t), closure, a_e expr new_env))
    | Type.Fun(list, tt) -> (match Typing.deref_type tt with
                        | Type.Int -> (if !rcounter < 2 then raise NoRegister else rcounter := !rcounter - 1;
                            let r = Register.make_register !rcounter in 
                            let new_env = Env.add env name r in
                            let closure = Closure.Cls((f_, t_), makeclosure ylst new_env) in
                            Closure.LetClosure ((r, tt), closure, a_e expr new_env))
                        | Type.Float -> (if !fcounter < 1 then raise NoRegister else fcounter := !fcounter - 1;
                            let r = Register.make_fregister !fcounter in 
                            let new_env = Env.add env name r in
                            let closure = Closure.Cls((f_, t_), makeclosure ylst new_env) in
                            Closure.LetClosure ((r, tt), closure, a_e expr new_env))
                        | _ -> raise TypeError)
    | _ -> raise TypeError)*)
  | Closure.AppC (name, args) -> Closure.AppC (name, args) 
  | Closure.AppD (name, args) -> Closure.AppD (name, args)
  with 
  | Env.UnboundVariable(name1) -> Closure.Variable(name1)


let rec envadd env ylst rlst = match ylst, rlst with
| [], [] -> env
| (y_first, t) :: y_rest, (r_first, t_r) :: r_rest -> Env.add (envadd env y_rest r_rest) y_first r_first
| _, _ -> raise ListError

let rec g_def (Closure.FunDef ((name, t), y_lst, r_lst, expr)) env =
  let (rnum, fnum) = count r_lst 1 0 in
  let newr_lst = make_registerlst y_lst rnum fnum in
  rcounter := 12;
  fcounter := 15;
  Closure.FunDef ((name, t), newr_lst, r_lst, a_e expr (envadd env y_lst newr_lst))

let g_program (Closure.Program (def_list, expr)) env = 
  let rec prolist list = match list with 
  | [] -> []
  | d :: rest -> (g_def d env) :: prolist rest in 
  rcounter := 12;
  fcounter := 15;
  Closure.Program (prolist def_list , a_e expr env)

(* Alloc.f : First.prog_t -> First.prog_t *)

(* 単純なレジスタ割り当て *)
let f program = g_program program []
