open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let variant_test1 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type exp = A of int | B of bool;;" in
  let _, _, _, tylenv, varenv = read_string_eval expr env tyenv tylenv varenv in
  let id_ty_vals, _, _, _, _ = read_string_eval "A 1;;" env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyVariant "exp", VariantV ("A", IntV 1))];
  let id_ty_vals, _, _, _, _ = read_string_eval "B true;;" env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyVariant "exp", VariantV ("B", BoolV true))]

let variant_test2 _ = (* same tag variant *)
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type exp1 = A of int | B of bool;;" in
  let _, _, _, tylenv, varenv = read_string_eval expr env tyenv tylenv varenv in
  let expr = "type exp2 = A of int | C of bool;;" in
  let _, _, _, tylenv, varenv = read_string_eval expr env tyenv tylenv varenv in
  let id_ty_vals, _, _, _, _ = read_string_eval "A 1;;" env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyVariant "exp2", VariantV ("A", IntV 1))]

let suite =
  "suite">:::["variant_test1">:: variant_test1;
              "variant_test2">:: variant_test2;]

let () = run_test_tt_main suite
