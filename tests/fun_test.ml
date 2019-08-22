open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let fun_test1 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "(fun x -> x + 1);;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyFun (TyInt, TyInt), ProcV (PVar "x", BinOp (Plus, Var "x", ILit 1), ref env))]

let fun_test2 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "(fun x -> x + 2) 1;;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 3)]

let fun_test3 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "(fun id x -> id x) (fun x -> x) 0;;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 0)]

let fun_test4 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "(fun (a, b) [c; d] -> a + b + c + d) (1, 2) [3; 4];;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 10)]

let suite =
  "suite">:::["fun_test1">:: fun_test1;
              "fun_test2">:: fun_test2;
              "fun_test3">:: fun_test3;
              "fun_test4">:: fun_test4;]

let () = run_test_tt_main suite
