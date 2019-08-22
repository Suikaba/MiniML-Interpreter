open Core
open Syntax
open Typing
open Eval

let read_ch_eval_print ~ic:ic ~env:env ~tyenv:tyenv tyvenv varenv =
  print_string "# ";
  Out_channel.flush stdout;
  let decl = Parser.toplevel Lexer.main ic in
  let id_tys, newtyenv, tyvenv, varenv = ty_decl tyenv tyvenv varenv decl in
  let id_vals, newenv = eval_decl env varenv decl in
  let id_tys = SM.of_alist_exn id_tys in
  let id_val_tys = List.map ~f:(fun (id, v) -> (id, v, SM.find_exn id_tys id)) id_vals in
  List.iter ~f:(fun (id, v, ty) -> Printf.printf "val %s : " id;
                                   pp_ty ty;
                                   print_string " = ";
                                   pp_val v; Out_channel.print_endline "";)
            id_val_tys;
  newenv, newtyenv, tyvenv, varenv

let rec read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv =
  try
    let ic = Lexing.from_channel In_channel.stdin in
    let newenv, newtyenv, tyvenv, varenv = read_ch_eval_print ~ic:ic ~env:env ~tyenv:tyenv tyvenv varenv in
    read_stdin_eval_print ~env:newenv ~tyenv:newtyenv tyvenv varenv
  with e -> err_handler env tyenv tyvenv varenv e
and read_file_eval_print fp ic env tyenv tyvenv varenv =
  try
    let newenv, newtyenv, tyvenv, varenv = read_ch_eval_print ~ic:ic ~env:env ~tyenv:tyenv tyvenv varenv in
    read_file_eval_print fp ic newenv newtyenv tyvenv varenv
  with e -> In_channel.close fp; err_handler env tyenv tyvenv varenv e
and err_handler env tyenv tyvenv varenv = function
  | Typing.Error msg -> print_endline msg; read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv
  | Eval.Error msg -> print_endline msg; read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv
  | Failure msg -> print_endline msg; read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv
  | _ -> print_endline "Fatal error"; read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv
and read_eval_print env tyenv tyvenv varenv =
  if Array.length Sys.argv > 1 then
    try
      let fp = In_channel.create Sys.argv.(1) in
      let ic = Lexing.from_channel fp in
      read_file_eval_print fp ic env tyenv tyvenv varenv
    with Sys_error msg -> print_endline msg; read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv
  else
    read_stdin_eval_print ~env:env ~tyenv:tyenv tyvenv varenv

(* pre-defined val *)
let not_function =
  ProcV (PVar "b", IfExp (Var("b"), BLit false, BLit true), ref Environment.empty)
let not_ty = TyFun (TyBool, TyBool)

let ref_function = ProcV (PVar "v", RefExp (Var "v"), ref Environment.empty)
let ref_tysc =
  let var = fresh_tyvar () in
  let tyvar = TyVar var in
  TyScheme ([var], TyFun (tyvar, TyRef tyvar))

let initial_env =
  Environment.extend "not" not_function
    (Environment.extend "ref" ref_function Environment.empty)
let initial_tyenv =
  Environment.extend "not" (tysc_of_ty not_ty)
    (Environment.extend "ref" ref_tysc Environment.empty)
let initial_tyvenv =
  Environment.extend "int" TyInt Environment.empty
  |> Environment.extend "bool" TyBool
  |> Environment.extend "unit" TyUnit
let initial_varenv = Environment.empty

(* for test *)
let read_string_eval str env tyenv tyvenv varenv =
  let decl = Parser.toplevel Lexer.main (Lexing.from_string str) in
  let id_tys, newtyenv, tyvenv, varenv = ty_decl tyenv tyvenv varenv decl in
  let id_vals, newenv = eval_decl env varenv decl in
  let id_tys = SM.of_alist_exn id_tys in
  let id_ty_vals = List.map id_vals
                     ~f:(fun (id, v) -> (id, SM.find_exn id_tys id, v)) in
  id_ty_vals, newenv, newtyenv, tyvenv, varenv
