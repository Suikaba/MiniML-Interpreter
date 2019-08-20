open Syntax
open Core

type exval =
    IntV of int
  | UnitV
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | RefV of exval ref
  | TupleV of exval list
  | ListV of exval list
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | UnitV -> "()"
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "<fun>"
  | RefV v -> "{contents = " ^ (string_of_exval !v) ^ "}"
  | TupleV vs -> "(" ^ (List.map ~f:(fun v -> string_of_exval v) vs |> String.concat ~sep:", ") ^ ")"
  | ListV vs -> "[" ^ (List.map ~f:(fun v -> string_of_exval v) vs |> String.concat ~sep:"; ") ^ "]"

let pp_val v = print_string (string_of_exval v)


(* =============================================================================
   Matching
 *)
let rec matching p v = match p, v with
    PVar id, v -> Some [(id, v)]
  | PUnitLit, UnitV -> Some []
  | PILit i, IntV j when i = j -> Some []
  | PBLit b1, BoolV b2 when b1 = b2 -> Some []
  | PTupleExp ps, TupleV vs ->
      let rec inner ps vs = (match ps, vs with
          [], [] -> Some []
        | _, [] | [], _ -> None
        | p :: p_tl, v :: v_tl ->
            let hd = matching p v in
            let tl = inner p_tl v_tl in
            (match hd, tl with
               Some hd, Some tl -> Some (hd @ tl)
             | _ -> None))
      in inner ps vs
  | PConsExp ps, ListV vs ->
      let rec inner ps vs = (match ps, vs with
          [], _ -> None
        | [p], tl -> matching p (ListV tl)
        | p :: p_tl, v :: v_tl ->
            let hd = matching p v in
            let tl = inner p_tl v_tl in
            (match hd, tl with
               Some hd, Some tl -> Some (hd @ tl)
             | _ -> None)
        | _, [] -> None)
      in inner ps vs
  | PListExp ps, ListV vs ->
      let rec inner ps vs = (match ps, vs with
          [], [] -> Some []
        | _, [] | [], _ -> None
        | p :: p_tl, v :: v_tl ->
            let hd = matching p v in
            let tl = inner p_tl v_tl in
            (match hd, tl with
               Some hd, Some tl -> Some (hd @ tl)
             | _ -> None))
      in inner ps vs
  | PCombineExp ps, v ->
      let rec inner = (function
          [] -> None
        | p :: ps ->
            (match matching p v with
               Some l -> Some l
             | None -> inner ps))
      in inner ps
  | _ -> None

(* =============================================================================
   Main
*)
let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Eq, BoolV b1, BoolV b2 -> BoolV (b1 = b2)
  | Assign, RefV r, arg2 -> r := arg2; UnitV
  | Cons, v, ListV vs -> ListV (v :: vs)
  | Append, ListV vs1, ListV vs2 -> ListV (vs1 @ vs2)
  | _ -> err "Runtime error: apply_prim"

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
    (* first, evaluate all expr with current environment *)
    let id_vals = List.map binds
                    ~f:(fun (p, e) -> match matching p (eval_exp env e) with
                          Some l -> l
                        | None -> err "Eval: matching error")
                  |> List.join in
    (* then, update environment *)
    let newenv = List.fold_left id_vals ~init:env
                   ~f:(fun e (id, v) -> Environment.extend id v e) in
    eval_exp newenv exp2
  | LetRecExp (binds, exp2) ->
      let dummyenv = ref Environment.empty in
      let make_dummy_proc = function
          FunExp (para, body) -> ProcV(para, body, dummyenv)
        | e -> eval_exp env e in (* if e has not function, e must not contain names defined now *)
      (* first, add all rec functions to current environment *)
      let newenv = List.fold_left binds ~init:env
                     ~f:(fun env (p, e) -> match matching p (make_dummy_proc e) with
                           Some l ->
                             List.fold_left l ~init:env
                               ~f:(fun env (id, v) -> Environment.extend id v env)
                         | None -> err "Eval: matching error") in
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
  | TupleExp exps -> TupleV (List.map ~f:(fun e -> eval_exp env e) exps)
  | ListExp exps -> ListV (List.map ~f:(fun e -> eval_exp env e) exps)
  | MatchExp (exp1, mexps) ->
      let v = eval_exp env exp1 in
      let rec inner = (function
          [] -> None
        | (p, exp2) :: tl ->
            (match matching p v with
               Some l ->
                 let env = List.fold_left l ~init:env
                             ~f:(fun env (id, v) -> Environment.extend id v env) in
                 Some (eval_exp env exp2)
             | None -> inner tl))
      in
      (match inner mexps with
         Some v -> v
       | None -> err "Eval: matching error")

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ([("-", v)], env)
  | Decl binds ->
      (* first, evaluate all expr with current environment *)
      let id_vals = List.map binds
                      ~f:(fun (p, e) -> match matching p (eval_exp env e) with
                            Some l -> l
                          | None -> err "Eval: matching error")
                    |> List.join in
      (* then, update environment *)
      let newenv = List.fold_left id_vals ~init:env
                     ~f:(fun e (id, v) -> Environment.extend id v e) in
      (id_vals, newenv)
  | RecDecl binds ->
      let dummyenv = ref Environment.empty in
      let make_dummy_proc = function
          FunExp (para, body) -> ProcV(para, body, dummyenv)
        | e -> eval_exp env e in (* if e has not function, e must not contain names defined now *)
      (* first, add all rec functions to current environment *)
      let id_vals, newenv =
        List.fold_right binds ~init:([], env)
          ~f:(fun (p, e) (id_vals, env) -> match matching p (make_dummy_proc e) with
                Some l ->
                  List.fold_right l ~init:(id_vals, env)
                    ~f:(fun (id, v) (id_vals, env) -> (id, v) :: id_vals, Environment.extend id v env)
              | None -> err "Eval: matching error") in
      dummyenv := newenv;
      (id_vals, newenv)
