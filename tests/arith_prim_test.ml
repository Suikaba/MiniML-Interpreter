open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let int_prim_test _ =
  let id_ty_vals,  _, _ = read_string_eval "2 + 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 5)];
  let id_ty_vals, _, _ = read_string_eval "2 - 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV (-1))];
  let id_ty_vals, _, _ = read_string_eval "2 * 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 6)];
  let id_ty_vals, _, _ = read_string_eval "2 / 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 0)];
  let id_ty_vals, _, _ = read_string_eval "2 * 3 + 4 / 2;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 8)];
  let id_ty_vals, _, _ = read_string_eval "2 < 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV true)];
  let id_ty_vals, _, _ = read_string_eval "3 < 3;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV false)]

let bool_prim_test _ =
  let id_ty_vals, _, _ = read_string_eval "true && true;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV true)];
  let id_ty_vals, _, _ = read_string_eval "false && true;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV false)];
  let id_ty_vals, _, _ = read_string_eval "false || false;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV false)];
  let id_ty_vals, _, _ = read_string_eval "true || false;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV true)];
  let id_ty_vals, _, _ = read_string_eval "true || true && false;;" initial_env initial_tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV true)]

let suite =
  "suite">:::["int_prim_test">:: int_prim_test;
              "bool_prim_test">:: bool_prim_test]

let () = run_test_tt_main suite
