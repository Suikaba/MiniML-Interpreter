(* ML interpreter / type reconstruction *)
open Core

type id = string

type binOp = Plus | Minus | Mult | Div | Lt | And | Or | Eq | Assign
             | Cons | Append

type patternExp =
    PVar of id
  | PUnitLit
  | PILit of int
  | PBLit of bool
  | PTupleExp of patternExp list
  | PConsExp of patternExp list
  | PListExp of patternExp list
  | PCombineExp of patternExp list

type exp =
    Var of id
  | UnitLit
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (patternExp * exp) list * exp
  | LetRecExp of (patternExp * exp) list * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | UnitSeqExp of exp * exp
  | RefExp of exp
  | DerefExp of exp
  | TupleExp of exp list
  | ListExp of exp list

(* for value restriction *)
let rec is_value_exp = function
  | UnitLit -> true
  | ILit _ -> true
  | FunExp _ -> true
  | BLit _ -> true
  | TupleExp exps
  | ListExp exps -> List.for_all exps ~f:(fun e -> is_value_exp e)
  | _ -> false

type program =
    Exp of exp
  | Decl of (patternExp * exp) list
  | RecDecl of (patternExp * exp) list

type tyvar = int
type ty =
    TyUnit
  | TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty
  | TyRef of ty
  | TyTuple of ty list

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)


module ST = Set.Make(Int)

let rec string_of_ty = function
    TyUnit -> "unit"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVar v -> "v" ^ (string_of_int v)
  | TyFun (ty1, ty2) ->
      (match ty1 with
         TyFun _ -> "(" ^ (string_of_ty ty1) ^ ") -> " ^ (string_of_ty ty2)
       | _ -> (string_of_ty ty1) ^ " -> " ^ (string_of_ty ty2))
  | TyRef ty -> (string_of_ty ty) ^ " ref"
  | TyTuple tys ->
      List.map tys ~f:(fun ty -> match ty with
                         TyFun _ | TyTuple _ -> "(" ^ (string_of_ty ty) ^ ")"
                       | _ -> string_of_ty ty)
      |> String.concat ~sep:" * "
  | TyList ty -> (string_of_ty ty) ^ " list"

let pp_ty ty = print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () = counter := !counter + 1; !counter in
  body

let rec freevar_ty = function
  | TyUnit | TyInt | TyBool -> ST.empty
  | TyVar v -> ST.singleton v
  | TyFun (ty1, ty2) -> ST.union (freevar_ty ty1) (freevar_ty ty2)
  | TyRef ty -> freevar_ty ty
  | TyTuple tys -> List.fold_right tys ~init:ST.empty ~f:(fun ty fv -> ST.union fv (freevar_ty ty))
  | TyList ty -> freevar_ty ty

let freevar_tysc tysc =
  let rec freevar_tysc_impl binds = (function
    | TyUnit | TyInt | TyBool -> ST.empty
    | TyVar v -> if List.exists binds ~f:(fun v' -> v = v') then ST.empty else ST.singleton v
    | TyFun (ty1, ty2) -> ST.union (freevar_tysc_impl binds ty1) (freevar_tysc_impl binds ty2)
    | TyRef ty -> freevar_tysc_impl binds ty
    | TyTuple tys -> List.fold_right tys ~init:ST.empty ~f:(fun ty fv -> ST.union fv (freevar_tysc_impl binds ty))
    | TyList ty -> freevar_tysc_impl binds ty)
  in
  match tysc with
  | TyScheme (binds, ty) -> freevar_tysc_impl binds ty

(* helper functions for parser *)

(* let {rec} id <args> = exp -> let {rec} id = fun <args> -> exp *)
let rec make_fun_exp exp = function
    [] -> exp
  | hd :: tl -> FunExp (hd, make_fun_exp exp tl)
