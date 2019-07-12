open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus | Minus | Mult | Div ->
    (match ty1, ty2 with
         TyInt, TyInt -> TyInt
       | _ -> err "Argument must be of integer")
  | Lt -> (* todo *)
    (match ty1, ty2 with
         TyInt, TyInt -> TyBool
       | _ -> err "Argument must be of boolean")
  | And | Or ->
    (match ty1, ty2 with
         TyBool, TyBool -> TyBool
       | _ -> err "Argument must be of boolean")
  | Eq ->
    (match ty1, ty2 with
         TyInt, TyInt -> TyBool
       | TyBool, TyBool -> TyBool
       | _ -> err "Argument must have same type")

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tytest = ty_exp tyenv exp1 in
      let ty2 = ty_exp tyenv exp2 in
      let ty3 = ty_exp tyenv exp3 in
      if tytest <> TyBool then err "test expression must be of boolean: if";
      if ty2 <> ty3 then err "then-else expression must have same type: if";
      ty2
  | LetExp (binds, exp2) ->
      let newenv = List.fold_left
                     (fun env' (id, exp1) -> Environment.extend id (ty_exp tyenv exp1) env')
                     tyenv binds in
      ty_exp newenv exp2
  | LetRecExp (binds, exp2) -> err "Not implemented"
  | FunExp (id, exp) -> err "Not implemented"
  | AppExp (exp1, exp2) -> err "Not implemented"

let ty_decl tyenv = function
    Exp e -> (["-", ty_exp tyenv e], tyenv)
  | Decl binds ->
      let id_tys = List.map (fun (id, e) -> (id, ty_exp tyenv e)) binds in
      let newenv = List.fold_left (fun env' (id, ty) -> Environment.extend id ty env') tyenv id_tys in
      (id_tys, newenv)
  | RecDecl binds -> err "Not implemented"
