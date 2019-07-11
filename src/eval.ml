open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* errors *)
let err_bound_several_times () = err "Variable is bound several times in this matching"

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "<fun>"

let pp_val v = print_string (string_of_exval v)


(* checking several times bindings *)
let rec check_bound_several_times = function
    [] -> false
  | (id1, _) :: tl -> List.exists (fun (id2, _) -> id1 = id2) tl || check_bound_several_times tl

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
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
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
         ProcV (id, body, env') ->
           let newenv = Environment.extend id arg env' in
           eval_exp newenv body
       | _ -> err ("Non-function value is applied"))

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ([("-", v)], env)
  | Decl binds ->
    if check_bound_several_times binds then err_bound_several_times ();
    (* first, evaluate all expr with current environment *)
    let id_vals = List.map (fun (id, e) -> (id, eval_exp env e)) binds in
    (* then, update environment *)
    let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env id_vals in
    (id_vals, newenv)
