open Syntax

type exval =
    IntV of int
  | UnitV
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | RefV of exval ref
  | TupleV of exval list
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* errors *)
let err_bound_several_times () = err "Variable is bound several times in this matching"
let err_let_rec_not_allowed () = err "This kind of expression is not allowed as right-hand side of 'let rec'"

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | UnitV -> "()"
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "<fun>"
  | RefV v -> "{contents = " ^ (string_of_exval !v) ^ "}"
  | TupleV vs -> "(" ^ (List.map (fun v -> string_of_exval v) vs |> String.concat ", ") ^ ")"

let pp_val v = print_string (string_of_exval v)


(* checking several times bindings *)
let rec check_bound_several_times = function
    [] -> false
  | (id1, _) :: tl -> List.exists (fun (id2, _) -> id1 = id2) tl || check_bound_several_times tl

(* checking invalid let rec *)
let check_let_rec binds =
  let rec check_impl name = function
      ILit _ | BLit _ | UnitLit -> false
    | Var x -> x = name
    | BinOp (_, exp1, exp2) -> (check_impl name exp1) || (check_impl name exp2)
    | IfExp (exp1, exp2, exp3) -> (check_impl name exp1) || (check_impl name exp2) || (check_impl name exp3)
    | LetExp (binds, exp2) | LetRecExp (binds, exp2) ->
        let appear = List.exists (fun (id, _) -> id = name) binds in
        not appear && check_impl name exp2
    | FunExp _ -> false (* todo: Is this correct ? *)
    | AppExp (exp1, exp2) -> (check_impl name exp1) || (check_impl name exp2)
    | UnitSeqExp (exp1, exp2) -> (check_impl name exp1) || (check_impl name exp2)
    | TupleExp exps -> List.exists (fun e -> check_impl name e) exps
    | _ -> err "check_let_rec: Not implemented"
  in
  List.map (fun (id, _) -> id) binds
  |> List.exists (fun name -> List.exists (fun (_, exp) -> check_impl name exp) binds)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
  | Div, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Eq, BoolV b1, BoolV b2 -> BoolV (b1 = b2)
  | Eq, _, _ -> err ("Both argument must have same type: =")
  | Assign, RefV r, arg2 -> r := arg2; UnitV
  | Assign, _, _ -> err "Must have reference type"

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | UnitLit -> UnitV
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (binds, exp2) ->
    if check_bound_several_times binds then err_bound_several_times ();
    (* first, evaluate all expr with current environment *)
    let id_vals = List.map (fun (id, e) -> (id, eval_exp env e)) binds in
    (* then, update environment *)
    let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env id_vals in
    eval_exp newenv exp2
  | LetRecExp (binds, exp2) ->
      if check_bound_several_times binds then err_bound_several_times ();
      if check_let_rec binds then err_let_rec_not_allowed ();
      let dummyenv = ref Environment.empty in
      let make_dummy_proc = function
          FunExp (para, body) -> ProcV(para, body, dummyenv)
        | e -> eval_exp env e in (* if e has not function, e must not contain names defined now *)
      (* first, add all rec functions to current environment *)
      let newenv = List.fold_left (fun env' (id, e) -> Environment.extend id (make_dummy_proc e) env') env binds in
      dummyenv := newenv;
      eval_exp newenv exp2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
         ProcV (id, body, env') ->
           let newenv = Environment.extend id arg !env' in
           eval_exp newenv body
       | _ -> err ("Non-function value is applied"))
  | UnitSeqExp (exp1, exp2) ->
      let _ = eval_exp env exp1 in
      eval_exp env exp2
  | RefExp exp ->
      let v = eval_exp env exp in
      RefV (ref v)
  | DerefExp exp ->
      (match eval_exp env exp with
       | RefV r -> !r
       | _ -> err "Eval/DerefExp: Non-reference type is dereferenced")
  | TupleExp exps -> TupleV (List.map (fun e -> eval_exp env e) exps)

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ([("-", v)], env)
  | Decl binds ->
    if check_bound_several_times binds then err_bound_several_times ();
    (* first, evaluate all expr with current environment *)
    let id_vals = List.map (fun (id, e) -> (id, eval_exp env e)) binds in
    (* then, update environment *)
    let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env id_vals in
    (id_vals, newenv)
  | RecDecl binds ->
    if check_bound_several_times binds then err_bound_several_times ();
    if check_let_rec binds then err_let_rec_not_allowed ();
    let dummyenv = ref Environment.empty in
    let make_dummy_proc = function
        FunExp (para, body) -> ProcV(para, body, dummyenv)
      | e -> eval_exp env e in (* if e has not function, e must not contain names defined now *)
    (* first, add all rec functions to current environment *)
    let newenv = List.fold_left (fun env' (id, e) -> Environment.extend id (make_dummy_proc e) env') env binds in
    dummyenv := newenv;
    let id_vals = List.map (fun (id, e) -> (id, eval_exp newenv e)) binds in
    (id_vals, newenv)
