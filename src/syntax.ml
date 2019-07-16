(* ML interpreter / type reconstruction *)
open Core
type id = string

type binOp = Plus | Minus | Mult | Div | Lt | And | Or | Eq | Assign

type exp =
    Var of id
  | UnitLit
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp
  | LetRecExp of (id * exp) list * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | UnitSeqExp of exp * exp
  | RefExp of exp
  | DerefExp of exp

type program =
    Exp of exp
  | Decl of (id * exp) list
  | RecDecl of (id * exp) list

type tyvar = int
type ty =
    TyUnit
  | TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty
  | TyRef of ty

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)


module ST = Set.Make(Int)

let rec pp_ty = function
    TyUnit -> print_string "unit"
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
  | TyRef ty -> pp_ty ty; print_string " ref"
  | _ -> print_string "Not implemented"

let fresh_tyvar =
  let counter = ref 0 in
  let body () = counter := !counter + 1; !counter in
  body

let rec freevar_ty = function
  | TyUnit | TyInt | TyBool -> ST.empty
  | TyVar v -> ST.singleton v
  | TyFun (ty1, ty2) -> ST.union (freevar_ty ty1) (freevar_ty ty2)
  | TyRef ty -> freevar_ty ty
  | TyList _ -> failwith "Not implemented"

let freevar_tysc tysc =
  let rec freevar_tysc_impl binds = (function
    | TyUnit | TyInt | TyBool -> ST.empty
    | TyVar v -> if List.exists binds ~f:(fun v' -> v = v') then ST.empty else ST.singleton v
    | TyFun (ty1, ty2) -> ST.union (freevar_tysc_impl binds ty1) (freevar_tysc_impl binds ty2)
    | TyRef ty -> freevar_tysc_impl binds ty
    | TyList _ -> failwith "Not implemented")
  in
  match tysc with
  | TyScheme (binds, ty) -> freevar_tysc_impl binds ty

(* helper functions for parser *)

(* let {rec} id <args> = exp -> let {rec} id = fun <args> -> exp *)
let rec make_fun_exp exp = function
    [] -> exp
  | hd :: tl -> FunExp (hd, make_fun_exp exp tl)
