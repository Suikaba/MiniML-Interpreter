open Core
open Syntax

exception Error of string
exception TypeMismatch of ty * ty

let err s = raise (Error s)
let type_mismatch ty1 ty2 = raise (TypeMismatch (ty1, ty2))

type tyenv = tysc Environment.t

(* ============================================================================
 * Unification
 *)
module IM = Int.Map
module UF = Unionfind

type subst = ty Unionfind.t IM.t

let print_subst s = Map.iteri s ~f:(fun ~key:k ~data:v -> Printf.printf "(%d, " k;
                                                          pp_ty (Unionfind.get_value v);
                                                          print_endline ")")
let print_eqs s =
  List.iter s ~f:(fun (ty1, ty2) -> pp_ty ty1; print_string " = "; pp_ty ty2)

let err_recursive_ftv () = err "unify: recursive FTV (@todo: better error message)"
let raise_unbound_constr constr = err ("Unbound constructor " ^ constr)

let rec freevar_tyenv tyenv =
  Environment.fold_right
    (fun tysc set -> ST.union set (freevar_tysc tysc))
    tyenv ST.empty

let rec subst_type s = function
  | TyVar v ->
    (match Map.find s v with
     | Some t ->
       (match Unionfind.get_value t with
        | TyVar v' when v = v' -> TyVar v
        | ty -> subst_type s ty)
     | None -> TyVar v)
  | TyFun (ty1, ty2) -> TyFun (subst_type s ty1, subst_type s ty2)
  | TyList ty -> TyList (subst_type s ty)
  | TyRef ty -> TyRef (subst_type s ty)
  | TyTuple tys -> TyTuple (List.map tys ~f:(fun ty -> subst_type s ty))
  | t -> t

(* t1 -> forall a_1, ..., a_k.t1 *)
let make_closure ~ty:ty ~tyenv:tyenv ~subst:subst =
  let fv_tyenv =
    ST.fold_right ~init:ST.empty (freevar_tyenv tyenv)
      ~f:(fun id set -> ST.union set (freevar_ty (subst_type subst (TyVar id))))
  in
  let ids = ST.diff (freevar_ty ty) fv_tyenv in
  TyScheme (ST.to_list ids, subst_type subst ty)

(* add equal relation ty1 = ty2 to type substitution *)
(* @param subst : current type substitution *)
(* @param eqs : relations ty1 = ty2 *)
(* @return : new type substitution *)
let rec unify subst eqs =
  (* if new tyvar appeared, extend type substitution *)
  let extend s = (function
    | TyVar v -> if IM.mem s v then s else IM.add_exn s ~key:v ~data:(UF.create (TyVar v))
    | _ -> s)
  in
  match eqs with
  | [] -> subst
  | (ty1, ty2) :: tl ->
    let subst = extend (extend subst ty1) ty2 in
    let unify_root ty1 ty2 =
      (match ty1, ty2 with
      | TyInt, TyInt -> unify subst tl
      | TyBool, TyBool -> unify subst tl
      | TyUnit, TyUnit -> unify subst tl
      | TyEmpty, TyEmpty -> unify subst tl
      | TyVar v1, TyVar v2 ->
          UF.union (Map.find_exn subst v1) (Map.find_exn subst v2);
          unify subst tl
      | TyVar v, t | t, TyVar v ->
          if ST.mem (freevar_ty t) v then err_recursive_ftv ();
          UF.set_value (Map.find_exn subst v) t;
          unify subst tl
      | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
          unify subst ((ty11, ty21) :: (ty12, ty22) :: tl)
      | TyRef ty1, TyRef ty2 -> unify subst ((ty1, ty2) :: tl)
      | TyTuple tys1, TyTuple tys2 when List.length tys1 = List.length tys2 ->
          unify subst ((List.zip_exn tys1 tys2) @ tl)
      | TyList ty1, TyList ty2 -> unify subst ((ty1, ty2) :: tl)
      | TyVariant id1, TyVariant id2 when id1 = id2 -> unify subst tl
      | _ -> type_mismatch ty1 ty2)
    in
    (match ty1, ty2 with
     | TyVar v1, TyVar v2 ->
         let t1 = Map.find_exn subst v1 in
         let t2 = Map.find_exn subst v2 in
         UF.union t1 t2;
         unify_root (UF.get_value t1) (UF.get_value t2)
     | TyVar v, t | t, TyVar v ->
         unify_root (UF.get_value (Map.find_exn subst v)) t
     | ty1, ty2 -> unify_root ty1 ty2)

let rec merge_subst s1 s2 =
  Map.fold_right ~init:s1 s2
    ~f:(fun ~key:v ~data:t2 subst ->
          if Map.mem subst v then begin
            let t1 = Option.value_exn (Map.find subst v) in
            let ty1 = UF.get_value t1 in
            let ty2 = UF.get_value t2 in
            UF.union t1 t2;
            unify subst [(ty1, ty2)]
          end else
            Map.add_exn subst ~key:v ~data:t2)


(* ============================================================================
   Matching
 *)
module SS = String.Set
module SM = String.Map

let rec pattern_variables = function
    PVar i -> SS.singleton i
  | PUnitLit | PILit _ | PBLit _ -> SS.empty
  | PTupleExp ps
  | PConsExp ps
  | PListExp ps ->
      let vs, sum = List.map ps ~f:(fun p -> pattern_variables p)
                    |> List.fold_left ~init:(SS.empty, 0)
                         ~f:(fun (vs', sum) vs -> SS.union vs' vs, sum + SS.length vs)
      in
      if SS.length vs <> sum then err "Variable is bound several times in this matching";
      vs
  | PConstrExp _ -> SS.empty
  | PConstrAppExp (_, p) -> pattern_variables p
  | PPlaceholderExp -> SS.empty
  | PCombineExp ps ->
      let vss = List.map ps ~f:(fun p -> pattern_variables p) in
      let fst = List.hd_exn vss in
      List.iter vss ~f:(fun vs -> if not (SS.equal fst vs) then
                                    err "Variable must occur on both sudes of this | pattern");
      fst

let rec ty_pattern penv varenv = function
    PVar v -> SM.find_exn penv v, []
  | PUnitLit -> TyUnit, []
  | PILit _ -> TyInt, []
  | PBLit _ -> TyBool, []
  | PTupleExp ps ->
      let tys, eqs = List.fold_right ps ~init:([], [])
                       ~f:(fun p (tys, eqs) ->
                             let ty, eqs' = ty_pattern penv varenv p in
                             (ty :: tys, eqs' @ eqs)) in
     TyTuple tys, eqs
  | PConsExp ps ->
      assert ((List.length ps) >= 2);
      let ty1, eqs1 = ty_pattern penv varenv (List.hd_exn ps) in
      let rec inner = (function
          [] -> err "ty_pattern, inner: Never reach"
        | [p] ->
            let tl_ty, eqs' = ty_pattern penv varenv p in
            (TyList ty1, tl_ty) :: eqs' @ eqs1
        | p :: ps ->
            let ty', eqs' = ty_pattern penv varenv p in
            (ty1, ty') :: eqs' @ inner ps)
     in
     TyList ty1, inner (List.tl_exn ps)
  | PListExp ps ->
      (match ps with
         [] -> TyVar (fresh_tyvar ()), []
       | p :: ps ->
           let ty1, eqs1 = ty_pattern penv varenv p in
           let eqs = List.fold_left ps ~init:eqs1
                       ~f:(fun eqs p ->
                             let ty', eqs' = ty_pattern penv varenv p in
                             (ty1, ty') :: eqs' @ eqs)
           in
           TyList ty1, eqs)
  | PConstrExp id ->
      let var_id, ty = (try Environment.lookup id varenv
                          with Environment.Not_bound -> raise_unbound_constr id) in
      TyVariant var_id, [ty, TyEmpty]
  | PConstrAppExp (id, p) ->
      let var_id, ty = (try Environment.lookup id varenv
                          with Environment.Not_bound -> raise_unbound_constr id) in
      let tyarg, eqs = ty_pattern penv varenv p in
      TyVariant var_id, (ty, tyarg) :: eqs
  | PPlaceholderExp -> TyVar (fresh_tyvar ()), []
  | PCombineExp ps ->
      let ty1, eqs = ty_pattern penv varenv (List.hd_exn ps) in
      ty1,
      List.fold_left (List.tl_exn ps) ~init:eqs
        ~f:(fun eqs p ->
              let ty', eqs' = ty_pattern penv varenv p in
              (ty1, ty') :: eqs' @ eqs)


(* ============================================================================
 * Evaluation
 *)

(* @todo: bad name... *)
let check_let_rec binds =
  let rec inner name = function
      ILit _ | BLit _ | UnitLit -> false
    | Var x -> x = name
    | BinOp (_, exp1, exp2) -> (inner name exp1) || (inner name exp2)
    | IfExp (exp1, exp2, exp3) -> (inner name exp1) || (inner name exp2) || (inner name exp3)
    | LetExp (binds, exp2) | LetRecExp (binds, exp2) ->
        let appear = List.exists binds
                       ~f:(fun (p, _) -> SS.mem (pattern_variables p) name)
        in not appear && inner name exp2
    | FunExp _ -> false (* todo: Is this correct ? *)
    | AppExp (exp1, exp2) -> (inner name exp1) || (inner name exp2)
    | UnitSeqExp (exp1, exp2) -> (inner name exp1) || (inner name exp2)
    | TupleExp exps -> List.exists ~f:(fun e -> inner name e) exps
    | RefExp exp -> inner name exp
    | DerefExp exp -> inner name exp
    | ConstrExp _ -> false
    | ConstrAppExp (_, exp) -> inner name exp
    | ListExp exps -> List.exists ~f:(fun e -> inner name e) exps
    | MatchExp _ -> err "Not implemented"
  in
  let is_invalid = List.map binds ~f:(fun (p, _) -> pattern_variables p |> SS.to_list)
                   |> List.join
                   |> List.exists ~f:(fun name -> List.exists ~f:(fun (_, exp) -> inner name exp) binds)
  in
  if is_invalid then
    err "Typing: This kind of expression is not allowed as right-hand side of 'let rec'"

let rec check_bound_several_times binds =
  let rec inner appeared = function
      [] -> ()
    | (p, _) :: tl ->
        let xs = pattern_variables p in
        if SS.exists xs ~f:(fun x -> SS.mem appeared x) then
          err "Variable is bound several times in this matching";
        inner (SS.union appeared xs) tl
  in
  inner SS.empty binds


let ty_prim op ty1 ty2 = match op with
  | Plus | Minus | Mult | Div -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool) (* todo *)
  | And | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Eq -> ([(ty1, ty2)], TyBool) (* todo *)
  | Assign -> ([ty1, TyRef ty2], TyUnit)
  | Cons -> ([TyList ty1, ty2], TyList ty1)
  | Append ->
      let ty_alpha = TyVar (fresh_tyvar ()) in
      [(ty1, TyList ty_alpha); (ty2, TyList ty_alpha)], TyList ty_alpha

let rec ty_exp tyenv varenv = function
    Var x ->
      (try
         let TyScheme (vars, ty) = Environment.lookup x tyenv in
         let s = List.fold_right ~init:IM.empty vars
                   ~f:(fun id s -> IM.add_exn s ~key:id ~data:(UF.create (TyVar (fresh_tyvar ()))))
         in
         (IM.empty, subst_type s ty)
       with Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> (IM.empty, TyInt)
  | BLit _ -> (IM.empty, TyBool)
  | UnitLit -> (IM.empty, TyUnit)
  | BinOp (op, exp1, exp2) ->
      let (s1, tyarg1) = ty_exp tyenv varenv exp1 in
      let (s2, tyarg2) = ty_exp tyenv varenv exp2 in
      let (eqs, tyans) = ty_prim op tyarg1 tyarg2 in
      let s = unify (merge_subst s1 s2) eqs in
      (s, subst_type s tyans)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, tytest) = ty_exp tyenv varenv exp1 in
      let (s2, ty2) = ty_exp tyenv varenv exp2 in
      let (s3, ty3) = ty_exp tyenv varenv exp3 in
      if tytest <> TyBool then err "test expression must be of boolean: if";
      let newsubst = unify (merge_subst s1 (merge_subst s2 s3)) [(tytest, TyBool); (ty2, ty3)] in
      (newsubst, subst_type newsubst ty2)
  | LetExp (binds, exp2) ->
      check_bound_several_times binds;
      let (s1, newtyenv) =
        List.fold_left binds ~init:(IM.empty, tyenv)
          ~f:(fun (s, env') (p, exp1) ->
                let s', ty = ty_exp tyenv varenv exp1 in
                let penv = SS.to_list (pattern_variables p)
                           |> List.map ~f:(fun id -> id, TyVar (fresh_tyvar ()))
                           |> SM.of_alist_exn in
                let p_ty, eqs = ty_pattern penv varenv p in
                let s' = unify s' ((p_ty, ty) :: eqs) in
                let TyScheme (xs, _) =
                  if is_value_exp exp1 then
                    make_closure ~ty:ty ~tyenv:tyenv ~subst:s'
                  else
                    tysc_of_ty ty
                in
                let newtyenv = SM.to_alist penv
                               |> List.fold_left ~init:env'
                                    ~f:(fun env' (id, ty) ->
                                          Environment.extend id (TyScheme (xs, subst_type s' ty)) env')
                in
                (merge_subst s s', newtyenv))
      in
      let (s2, tyans) = ty_exp newtyenv varenv exp2 in
      let newsubst = merge_subst s1 s2 in
      (newsubst, subst_type newsubst tyans)
  | LetRecExp (binds, exp2) ->
      check_bound_several_times binds;
      check_let_rec binds;
      let penv = List.map binds ~f:(fun (p, _) -> pattern_variables p |> SS.to_list)
                 |> List.join |> List.dedup_and_sort ~compare:String.compare
                 |> List.map ~f:(fun id -> id, TyVar (fresh_tyvar ()))
                 |> SM.of_alist_exn in
      let tmpenv = SM.fold penv ~init:tyenv
                     ~f:(fun ~key:id ~data:ty env' -> Environment.extend id (tysc_of_ty ty) env')
      in
      let (p_tys, s1) = List.fold_left binds ~init:([], IM.empty)
                           ~f:(fun (p_tys', s') (p, exp) ->
                                 let (s'', ty'') = ty_exp tmpenv varenv exp in
                                 let p_ty, eqs = ty_pattern penv varenv p in
                                 ((p, ty'', is_value_exp exp) :: p_tys',
                                  unify (merge_subst s' s'') ((ty'', p_ty) :: eqs)))
      in
      let newtyenv =
        List.fold_right p_tys ~init:tyenv
          ~f:(fun (p, ty, is_value) env ->
                let TyScheme (xs, _) = if is_value then
                                         make_closure ~ty:ty ~tyenv:tyenv ~subst:s1
                                       else
                                         tysc_of_ty ty in
                pattern_variables p |> SS.to_list
                |> List.fold_left ~init:env
                     ~f:(fun env id ->
                           Environment.extend id (TyScheme (xs, subst_type s1 (SM.find_exn penv id))) env)) in
      let (s2, tyans) = ty_exp newtyenv varenv exp2 in
      let s = merge_subst s1 s2 in
      (s, subst_type s tyans)
  | FunExp (p, exp) ->
      let penv = pattern_variables p |> SS.to_list
                 |> List.map ~f:(fun id -> id, TyVar (fresh_tyvar ()))
                 |> SM.of_alist_exn in
      let p_ty, eqs = ty_pattern penv varenv p in
      let tyenv = SM.fold penv ~init:tyenv
                       ~f:(fun ~key:id ~data:ty tyenv ->
                             Environment.extend id (tysc_of_ty ty) tyenv) in
      let s, ranty = ty_exp tyenv varenv exp in
      let s = unify s eqs in
      (s, TyFun (subst_type s p_ty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, tyf) = ty_exp tyenv varenv exp1 in
      let (s2, tyarg) = ty_exp tyenv varenv exp2 in
      let tybody = TyVar (fresh_tyvar ()) in
      let newsubst = unify (merge_subst s1 s2) [(tyf, TyFun (tyarg, tybody))] in
      (newsubst, subst_type newsubst tybody)
  | UnitSeqExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv varenv exp1 in
      if ty1 <> TyUnit then print_endline "Warning: this expression should have type unit.";
      let (s2, ty2) = ty_exp tyenv varenv exp2 in
      (merge_subst s1 s2, ty2)
  | RefExp _ -> err "ty_exp: input expression has RefExp"
  | DerefExp exp ->
      let s, ty = ty_exp tyenv varenv exp in
      let tyvar = TyVar (fresh_tyvar ()) in
      let newsubst = unify s [(ty, TyRef tyvar)] in
      (newsubst, subst_type newsubst tyvar)
  | TupleExp exps ->
      let s, tys = List.fold_right exps ~init:(IM.empty, [])
                     ~f:(fun e (s, tys) ->
                           let s', ty = ty_exp tyenv varenv e in
                           (merge_subst s s', ty :: tys))
      in
      (s, TyTuple (List.map tys ~f:(fun ty -> subst_type s ty)))
  | ListExp exps ->
      (match exps with
         [] -> (IM.empty, TyList (TyVar (fresh_tyvar ())))
       | exps ->
           let s1, ty1 = ty_exp tyenv varenv (List.hd_exn exps) in
           let s, eqs = List.fold_left (List.tl_exn exps) ~init:(s1, [])
                          ~f:(fun (s', eqs') e ->
                                let s'', ty'' = ty_exp tyenv varenv e in
                                (merge_subst s' s'', (ty1, ty'') :: eqs')) in
           let s = unify s eqs in
           (s, TyList (subst_type s ty1)))
  | MatchExp (e, mexps) ->
      let s, ty = ty_exp tyenv varenv e in
      let tys = List.map mexps
                  ~f:(fun (p, exp) ->
                        let penv = pattern_variables p |> SS.to_list
                                   |> List.map ~f:(fun id -> id, (TyVar (fresh_tyvar ())))
                                   |> SM.of_alist_exn
                        in
                        let p_ty, eqs = ty_pattern penv varenv p in
                        let s = unify s ((p_ty, ty) :: eqs) in
                        let tyenv = SM.to_alist penv
                                    |> List.fold_left ~init:tyenv
                                         ~f:(fun tyenv (id, ty) ->
                                               Environment.extend id (tysc_of_ty (subst_type s ty)) tyenv)
                        in
                        let s', ty' = ty_exp tyenv varenv exp in
                        subst_type (merge_subst s s') ty')
      in
      let s = List.map (List.tl_exn tys) ~f:(fun ty -> ty, List.hd_exn tys)
              |> unify s in
      s, subst_type s (List.hd_exn tys)
  | ConstrExp id ->
      let vid, vty = (try Environment.lookup id varenv
                      with Environment.Not_bound -> err ("Unbound constr: " ^ id)) in
      (match vty with
         TyEmpty -> IM.empty, TyVariant vid
       | _ -> err ("The constructor " ^ vid ^ " expects some arguments"))
  | ConstrAppExp (id, e) ->
      let vid, vty = (try Environment.lookup id varenv
                      with Environment.Not_bound -> err ("Unbound constr: " ^ id)) in
      let s, ty = ty_exp tyenv varenv e in
      unify s [vty, ty], TyVariant vid


let rec ty_texp tylenv = function
    TEVar id ->
      (try Environment.lookup id tylenv
       with Environment.Not_bound -> err ("Unbound type constructor " ^ id))
  | TEFun (texp1, texp2) -> TyFun (ty_texp tylenv texp1, ty_texp tylenv texp2)
  | TETuple texps -> TyTuple (List.map texps ~f:(fun texp -> ty_texp tylenv texp))
  | TEConstr (texp, id) when id = "list" -> TyList (ty_texp tylenv texp)
  | TEConstr (texp, id) when id = "ref" -> TyRef (ty_texp tylenv texp)
  | TEEmpty -> TyEmpty
  | _ -> err "error: ty_texp"


let type_definition tyvenv varenv decls =
  let rec check_multiple names = (function
    | [] -> ()
    | (name, constrs) :: tl ->
        if List.mem names name ~equal:String.equal then
          err ("Multiple definition of the type name " ^ name);
        let rec check_constr_multiple names = (function
          | [] -> ()
          | (name, _) :: tl ->
              if List.mem names name ~equal:String.equal then
                err ("Error: two constructors are named " ^ name);
              check_constr_multiple (name :: names) tl)
        in
        check_constr_multiple [] constrs;
        check_multiple (name :: names) tl)
  in
  check_multiple [] decls;
  let tyvenv = List.fold_left decls ~init:tyvenv
                 ~f:(fun tyenv (id, _) -> Environment.extend id (TyVariant id) tyenv) in
  let varenv = List.fold_left decls ~init:varenv
                 ~f:(fun varenv (name, id_texps) ->
                       List.fold_left id_texps ~init:varenv
                         ~f:(fun varenv (id, texp) ->
                               Environment.extend id (name, (ty_texp tyvenv texp)) varenv))
  in
  tyvenv, varenv

let ty_decl tyenv tyvenv varenv decl =
  let inner tyenv = (function
      Exp e ->
        let s, ty = ty_exp tyenv varenv e in
        (s, ["-", ty], tyenv, tyvenv, varenv)
    | Decl binds ->
        check_bound_several_times binds;
        let (s, id_tys, tyenv) =
          List.fold_left ~init:(IM.empty, [], tyenv) binds
            ~f:(fun (s', id_tys, tyenv) (p, exp) ->
                  let s, ty = ty_exp tyenv varenv exp in
                  let penv = pattern_variables p |> SS.to_list
                             |> List.map ~f:(fun id -> id, TyVar (fresh_tyvar ()))
                             |> SM.of_alist_exn in
                  let p_ty, eqs = ty_pattern penv varenv p in
                  let TyScheme (xs, _) =
                    if is_value_exp exp then
                      make_closure ~ty:ty ~tyenv:tyenv ~subst:s
                    else
                      tysc_of_ty ty
                  in
                  let s = unify (merge_subst s s') ((ty, p_ty) :: eqs) in
                  let id_tys, newtyenv =
                    SM.to_alist penv
                    |> List.fold_right ~init:(id_tys, tyenv)
                          ~f:(fun (id, ty) (id_tys, env') ->
                                (id, subst_type s ty) :: id_tys,
                                 Environment.extend id (TyScheme (xs, subst_type s ty)) env')
                  in
                  (s, id_tys, newtyenv))
        in
        s, id_tys, tyenv, tyvenv, varenv
    | RecDecl binds ->
        check_bound_several_times binds;
        check_let_rec binds;
        let penv = List.map binds ~f:(fun (p, _) -> pattern_variables p |> SS.to_list)
                   |> List.join |> List.map ~f:(fun id -> id, TyVar (fresh_tyvar ()))
                   |> SM.of_alist_exn in
        let tmpenv = SM.fold penv ~init:tyenv
                       ~f:(fun ~key:id ~data:ty tyenv -> Environment.extend id (tysc_of_ty ty) tyenv)
        in
        let p_tys, s = List.fold_left binds ~init:([], IM.empty)
                         ~f:(fun (p_tys, s) (p, exp) ->
                               let s', ty' = ty_exp tmpenv varenv exp in
                               let p_ty, eqs = ty_pattern penv varenv p in
                               ((p, ty', is_value_exp exp) :: p_tys,
                                unify (merge_subst s s') ((ty', p_ty) :: eqs)))
        in
        let tyenv =
          List.fold_right p_tys ~init:tyenv
            ~f:(fun (p, ty, is_value) tyenv ->
                  let TyScheme (xs, _) = if is_value then
                                            make_closure ~ty:ty ~tyenv:tyenv ~subst:s
                                          else
                                            tysc_of_ty ty
                  in
                  pattern_variables p |> SS.to_list
                  |> List.fold_left ~init:tyenv
                       ~f:(fun tyenv id ->
                             Environment.extend id (TyScheme (xs, subst_type s (SM.find_exn penv id))) tyenv))
        in
        let id_tys = SM.map penv ~f:(fun ty -> subst_type s ty) |> SM.to_alist in
        (s, id_tys, tyenv, tyvenv, varenv)
    | TyDef decls ->
        let tyvenv, varenv = type_definition tyvenv varenv decls in
        IM.empty, [], tyenv, tyvenv, varenv (* todo *))
  in
  let s, id_tys, tyenv, tyvenv, varenv = inner tyenv decl in
  (* update tyenv for weak type variables *)
  let tyenv = Environment.map (fun (TyScheme (xs, ty)) -> TyScheme (xs, subst_type s ty)) tyenv in
  (id_tys, tyenv, tyvenv, varenv)
