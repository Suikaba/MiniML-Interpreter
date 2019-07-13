open Core
open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

(* ============================================================================
 * Unification
 *)
module IM = Int.Map

type subst = ty Union_find.t IM.t

let print_subst s = Map.iteri s ~f:(fun ~key:k ~data:v -> Printf.printf "(%d, " k;
                                                          pp_ty (Unionfind.get_value v);
                                                          print_endline ")")

let err_recursive_ftv () = err "unify: recursive FTV (@todo: better error message)"

let rec subst_type s = function
  | TyVar v ->
    (match Map.find s v with
     | Some t ->
       (match Unionfind.get_value t with
        | TyVar v' when v = v' -> TyVar v
        | ty -> subst_type s ty)
     | None -> TyVar v)
  | TyFun (ty1, ty2) -> TyFun (subst_type s ty1, subst_type s ty2)
  | TyList _ -> failwith "Not implemented"
  | t -> t

let rec unify subst ty1 ty2 =
  let find_uf key =
    (match Map.find subst key with
     | Some t -> t
     | None -> failwith "unify:find_uf: internal error (never reach)")
  in
  let unify_root ty1 ty2 =
    (match ty1, ty2 with
    | TyVar v1, TyVar v2 -> Unionfind.union (find_uf v1) (find_uf v2) (fun x _ -> x)
    | TyVar v, t | t, TyVar v ->
      if ST.mem (free_ty_vars t) v then err_recursive_ftv ();
      Unionfind.set_value (find_uf v) t;
    | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
      unify subst ty11 ty21;
      unify subst ty12 ty22
    | TyBool, TyBool -> ()
    | TyInt, TyInt -> ()
    | _ -> err "Type mismatch")
  in
  match ty1, ty2 with
  | TyVar v1, TyVar v2 ->
    let t1 = find_uf v1 in
    let t2 = find_uf v2 in
    unify_root (Unionfind.get_value t1) (Unionfind.get_value t2);
    Unionfind.union t1 t2 (fun x _ -> x)
  | TyVar v, t | t, TyVar v ->
    unify_root (Unionfind.get_value (find_uf v)) t
  | ty1, ty2 -> unify_root ty1 ty2

let merge_subst s1 s2 =
  Map.fold s1 ~init:s2
           ~f:(fun ~key:k ~data:v s -> if Map.mem s k then s else Map.add_exn s ~key:k ~data:v)


(* ============================================================================
 * Evaluation
 *)

let ty_prim op ty1 ty2 subst = match op with
  | Plus | Minus | Mult | Div ->
    unify subst ty1 TyInt;
    unify subst ty2 TyInt;
    (subst, TyInt)
  | Lt -> (* todo *)
    unify subst ty1 TyInt;
    unify subst ty2 TyInt;
    (subst, TyBool)
  | And | Or ->
    unify subst ty1 TyBool;
    unify subst ty2 TyBool;
    (subst, TyBool)
  | Eq -> (* todo *)
    unify subst ty1 ty2;
    (subst, TyBool)

let rec ty_exp tyenv subst = function
    Var x ->
      (try (subst, subst_type subst (Environment.lookup x tyenv)) with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> (subst, TyInt)
  | BLit _ -> (subst, TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, tyarg1) = ty_exp tyenv subst exp1 in
      let (s2, tyarg2) = ty_exp tyenv subst exp2 in
      ty_prim op tyarg1 tyarg2 (merge_subst s1 s2)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, tytest) = ty_exp tyenv subst exp1 in
      let (s2, ty2) = ty_exp tyenv subst exp2 in
      let (s3, ty3) = ty_exp tyenv subst exp3 in
      if tytest <> TyBool then err "test expression must be of boolean: if";
      let newsubst = merge_subst s1 (merge_subst s2 s3) in
      unify newsubst ty2 ty3;
      (newsubst, subst_type newsubst ty2)
  | LetExp (binds, exp2) ->
      let (s1, newtyenv) =
        List.fold_left binds ~init:(subst, tyenv)
                     ~f:(fun (s, env') (id, exp1) ->
                           let (s', ty) = ty_exp tyenv s exp1 in
                           (merge_subst s s', Environment.extend id ty env'))
      in
      let (s2, tyans) = ty_exp newtyenv s1 exp2 in
      let newsubst = merge_subst s1 s2 in
      (newsubst, subst_type newsubst tyans)
  | LetRecExp (binds, exp2) -> err "Not implemented"
  | FunExp (id, exp) ->
      let vid = fresh_tyvar () in
      let domty = TyVar vid in
      let subst' = Map.add_exn subst ~key:vid ~data:(Unionfind.create domty) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) subst' exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, tyf) = ty_exp tyenv subst exp1 in
      let (s2, tyarg) = ty_exp tyenv subst exp2 in
      let body_v = fresh_tyvar () in
      let tybody = TyVar body_v in
      let s1 = Map.add_exn s1 ~key:body_v ~data:(Unionfind.create tybody) in
      let newsubst = merge_subst s1 s2 in
      unify newsubst tyf (TyFun (tyarg, tybody));
      (newsubst, subst_type newsubst tybody)
let ty_decl tyenv = function
    Exp e -> (["-", snd (ty_exp tyenv IM.empty e)], tyenv)
  | Decl binds ->
      let id_tys = List.map binds
                            ~f:(fun (id, e) ->
                                  let (_, ty) = ty_exp tyenv IM.empty e in
                                  (id, ty))
      in
      let newenv = List.fold_left id_tys ~init:tyenv ~f:(fun env' (id, ty) -> Environment.extend id ty env')
      in (id_tys, newenv)
  | RecDecl binds -> err "Not implemented"
