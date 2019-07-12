(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Minus | Mult | Div | Lt | And | Or | Eq

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp
  | LetRecExp of (id * exp) list * exp
  | FunExp of id * exp
  | AppExp of exp * exp

type program =
    Exp of exp
  | Decl of (id * exp) list
  | RecDecl of (id * exp) list

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | _ -> print_string "Not implemented"


(* helper functions for parser *)

(* let {rec} id <args> = exp -> let {rec} id = fun <args> -> exp *)
let rec make_fun_exp exp = function
    [] -> exp
  | hd :: tl -> FunExp (hd, make_fun_exp exp tl)
