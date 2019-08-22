
type binOp = Plus | Minus | Mult | Eq | Lt
;;

type exp =
  | VarIdx of int
  | ILit of int
  | BLit of bool
  | IfExp of exp * exp * exp
  | FunExp of int * exp
  | AppExp of exp * exp
  | LetExp of int * exp * exp
  | LetRecExp of int * int * exp * exp
  | BinOp of binOp * exp * exp
;;


type value =
    IntV of int
  | BoolV of bool
  | ProcV of int * exp * ((int * value) list ref)
;;

type val_optional =
    Some of value
  | None
;;

let rec lookup id env = match env with
  | [] -> None
  | (id', v) :: tl ->
      if id = id' then Some v
      else lookup id tl
;;

let extend id v env = (id, v) :: env
;;

let eval_binop op v1 v2 = match op, v1, v2 with
    Plus, IntV i1, IntV i2 -> Some (IntV (i1 + i2))
  | Minus, IntV i1, IntV i2 -> Some (IntV (i1 - i2))
  | Mult, IntV i1, IntV i2 -> Some (IntV (i1 * i2))
  | Eq, IntV i1, IntV i2 -> Some (BoolV (i1 = i2))
  | Lt, IntV i1, IntV i2 -> Some (BoolV (i1 < i2))
  | _ -> None
;;

let rec eval_exp env e = match e with
  | VarIdx id -> lookup id env
  | ILit i -> Some (IntV i)
  | BLit b -> Some (BoolV b)
  | BinOp (op, exp1, exp2) ->
      (match eval_exp env exp1, eval_exp env exp2 with
       | Some v1, Some v2 -> eval_binop op v1 v2
       | _ -> None)
  | IfExp (e1, e2, e3) ->
      (match eval_exp env e1 with
         Some (BoolV b) ->
             if b then eval_exp env e2
             else eval_exp env e3
       | _ -> None)
  | FunExp (id, body) -> Some (ProcV (id, body, ref env))
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval, arg with
         Some (ProcV (id, body, env)), Some v ->
           let env = extend id v !env in
           eval_exp env body
       | _ -> None)
  | LetExp (id, exp1, exp2) ->
      (match eval_exp env exp1 with
         Some v1 ->
           let env = extend id v1 env in
           eval_exp env exp2
       | None -> None)
  | LetRecExp (id, para, exp1, exp2) ->
      let dummy = ref [] in
      let newenv = extend id (ProcV (para, exp1, dummy)) env in
      dummy := newenv;
      eval_exp newenv exp2
;;


(* 1 + 2 *)
let exp1 = BinOp (Plus, ILit 1, ILit 2);;
eval_exp [] exp1;;

(* 1 + true *)
let exp1' = BinOp (Plus, ILit 1, BLit true);;
eval_exp [] exp1';;

(* let x = 1 in
   let y = 2 in
   x + y
*)
let exp2 = LetExp (0, ILit 1, LetExp (1, ILit 2, BinOp (Plus, VarIdx 0, VarIdx 1)));;
eval_exp [] exp2;;

(*
  let rec fact n =
    if n = 0 then 1 else n * (fact (n - 1))
  in fact 10
 *)
let exp3 = LetRecExp(0, 1,
                     IfExp (BinOp (Eq, VarIdx 1, ILit 0),
                            ILit 1,
                            BinOp (Mult,
                                   VarIdx 1,
                                   AppExp (VarIdx 0,
                                           BinOp (Minus, VarIdx 1, ILit 1)))),
                     AppExp (VarIdx 0, ILit 10));;
eval_exp [] exp3;;
