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

(* add equal relation ty1 = ty2 to type substitution *)
(* @param subst : current type substitution *)
(* @param eqs : relations ty1 = ty2 *)
(* @return : new type substitution *)
let rec unify subst eqs =
  (* if new tyvar appeared, extend type substitution *)
  let extend s = (function
    | TyVar v -> if IM.mem s v then s else IM.add_exn s ~key:v ~data:(Unionfind.create (TyVar v))
    | _ -> s)
  in
  let find_uf subst key =
    (match Map.find subst key with
     | Some t -> t
     | None -> failwith "unify:find_uf: internal error (never reach)")
  in
  match eqs with
  | [] -> subst
  | (ty1, ty2) :: tl ->
    let subst = extend (extend subst ty1) ty2 in
    let unify_root ty1 ty2 =
      (match ty1, ty2 with
      | TyVar v1, TyVar v2 ->
          Unionfind.union (find_uf subst v1) (find_uf subst v2);
          unify subst tl
      | TyVar v, t | t, TyVar v ->
          if ST.mem (free_ty_vars t) v then err_recursive_ftv ();
          Unionfind.set_value (find_uf subst v) t;
          unify subst tl
      | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
          unify subst ((ty11, ty21) :: (ty12, ty22) :: tl)
      | TyBool, TyBool -> unify subst tl
      | TyInt, TyInt -> unify subst tl
      | _ -> err "Type mismatch")
    in
    (match ty1, ty2 with
     | TyVar v1, TyVar v2 ->
         let t1 = find_uf subst v1 in
         let t2 = find_uf subst v2 in
         let ty1 = Unionfind.get_value t1 in
         let ty2 = Unionfind.get_value t2 in
         Unionfind.union t1 t2;
         unify_root ty1 ty2
     | TyVar v, t | t, TyVar v ->
       unify_root (Unionfind.get_value (find_uf subst v)) t
     | ty1, ty2 -> unify_root ty1 ty2)

let rec merge_subst s1 s2 =
  Map.fold_right ~init:s1 s2
    ~f:(fun ~key:v ~data:t2 subst ->
          if Map.mem subst v then begin
            let t1 = Option.value_exn (Map.find subst v) in
            let ty1 = Unionfind.get_value t1 in
            let ty2 = Unionfind.get_value t2 in
            Unionfind.union t1 t2;
            unify subst [(ty1, ty2)]
          end else
            Map.add_exn subst ~key:v ~data:t2)


(* ============================================================================
 * Evaluation
 *)

let ty_prim op ty1 ty2 = match op with
  | Plus | Minus | Mult | Div -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool) (* todo *)
  | And | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Eq -> ([(ty1, ty2)], TyBool) (* todo *)

let rec ty_exp tyenv = function
    Var x ->
      (try (IM.empty, Environment.lookup x tyenv) with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> (IM.empty, TyInt)
  | BLit _ -> (IM.empty, TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, tyarg1) = ty_exp tyenv exp1 in
      let (s2, tyarg2) = ty_exp tyenv exp2 in
      let (eqs, tyans) = ty_prim op tyarg1 tyarg2 in
      let s = unify (merge_subst s1 s2) eqs in
      (s, subst_type s tyans)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, tytest) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      if tytest <> TyBool then err "test expression must be of boolean: if";
      let newsubst = unify (merge_subst s1 (merge_subst s2 s3)) [(tytest, TyBool); (ty2, ty3)] in
      (newsubst, subst_type newsubst ty2)
  | LetExp (binds, exp2) ->
      let (s1, newtyenv) =
        List.fold_left binds ~init:(IM.empty, tyenv)
          ~f:(fun (s, env') (id, exp1) ->
                let (s', ty) = ty_exp tyenv exp1 in
                (merge_subst s s', Environment.extend id ty env'))
      in
      let (s2, tyans) = ty_exp newtyenv exp2 in
      let newsubst = merge_subst s1 s2 in
      (newsubst, subst_type newsubst tyans)
  | LetRecExp (binds, exp2) ->
      let newenv = List.fold_right binds ~init:tyenv
                     ~f:(fun (id, _) env' -> Environment.extend id (TyVar (fresh_tyvar ())) env')
      in
      let s1 = List.fold_right binds ~init:IM.empty
                 ~f:(fun (id, e) s' ->
                       let (s'', ty'') = ty_exp newenv e in
                       unify (merge_subst s' s'') [(Environment.lookup id newenv), ty''])
      in
      let (s2, tyans) = ty_exp newenv exp2 in
      let s = merge_subst s1 s2 in
      (s, subst_type s tyans)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, tyf) = ty_exp tyenv exp1 in
      let (s2, tyarg) = ty_exp tyenv exp2 in
      let tybody = TyVar (fresh_tyvar ()) in
      let newsubst = unify (merge_subst s1 s2) [(tyf, TyFun (tyarg, tybody))] in
      (newsubst, subst_type newsubst tybody)
let ty_decl tyenv = function
    Exp e -> (["-", snd (ty_exp tyenv e)], tyenv)
  | Decl binds ->
      let id_tys = List.map binds
                     ~f:(fun (id, e) ->
                           let (_, ty) = ty_exp tyenv e in
                           (id, ty))
      in
      let newenv = List.fold_left id_tys ~init:tyenv ~f:(fun env' (id, ty) -> Environment.extend id ty env')
      in (id_tys, newenv)
  | RecDecl binds ->
      let tmpenv = List.fold_left binds ~init:tyenv
                     ~f:(fun env' (id, _) -> Environment.extend id (TyVar (fresh_tyvar ())) env')
      in
      let id_tys = List.map binds ~f:(fun (id, e) -> (id, ty_exp tmpenv e)) in
      let subst = List.fold_left id_tys ~init:IM.empty ~f:(fun s (_, (s', _)) -> merge_subst s s') in
      List.fold_right id_tys ~init:([], tyenv)
        ~f:(fun (id, (_, ty)) (id_tys', env') ->
              let ty = subst_type subst ty in
              ((id, ty) :: id_tys', Environment.extend id ty env'))
