(* ML interpreter / type reconstruction *)
open Core
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

module ST = Set.Make(Int)

let rec pp_ty = function
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar v -> Printf.printf "v%d" v
  | TyFun (ty1, ty2) ->
      (match ty1 with
       | TyFun (_, _) -> print_string "(";
                         pp_ty ty1;
                         print_string ") -> ";
                         pp_ty ty2
       | _ -> pp_ty ty1; print_string " -> "; pp_ty ty2)
  | _ -> print_string "Not implemented"

let fresh_tyvar =
  let counter = ref 0 in
  let body () = counter := !counter + 1; !counter in
  body

let rec free_ty_vars = function
  | TyInt | TyBool -> ST.empty
  | TyVar v -> ST.singleton v
  | TyFun (ty1, ty2) -> ST.union (free_ty_vars ty1) (free_ty_vars ty2)
  | TyList _ -> failwith "Not implemented"

(* helper functions for parser *)

(* let {rec} id <args> = exp -> let {rec} id = fun <args> -> exp *)
let rec make_fun_exp exp = function
    [] -> exp
  | hd :: tl -> FunExp (hd, make_fun_exp exp tl)
