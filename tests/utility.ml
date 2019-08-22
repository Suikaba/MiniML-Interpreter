open Core
open Miniml.Cui
open Miniml.Syntax

let rec is_equal_ty ty1 ty2 = match ty1, ty2 with
  | TyVar _, TyVar _
  | TyInt, TyInt
  | TyBool, TyBool
  | TyUnit, TyUnit -> true
  | TyFun (arg1, body1), TyFun (arg2, body2) ->
      is_equal_ty arg1 arg2 && is_equal_ty body1 body2
  | TyList ty1, TyList ty2 -> is_equal_ty ty1 ty2
  | TyRef ty1, TyRef ty2 -> is_equal_ty ty1 ty2
  | TyTuple tys1, TyTuple tys2 ->
      List.length tys1 = List.length tys2
      && List.for_all (List.zip_exn tys1 tys2) ~f:(fun (ty1, ty2) -> is_equal_ty ty1 ty2)
  | _ -> false

let rec is_equal res1 res2 = match res1, res2 with
  | [], [] -> true
  | _, [] | [], _ -> false
  | (id1, ty1, v1) :: tl1, (id2, ty2, v2) :: tl2 ->
      id1 = id2 && is_equal_ty ty1 ty2 && v1 = v2
      && is_equal tl1 tl2
