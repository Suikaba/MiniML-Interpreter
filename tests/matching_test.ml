open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let matching_test1 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "match (1, 2) with
                | (1, 1) -> 1
                | (1, 2) -> 2;;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 2)]

let matching_test2 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "let rec sum l = match l with
                | [] -> 0
                | hd :: tl -> hd + (sum tl)
              in sum [1; 2; 3; 4; 5];;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 15)]

let matching_test3 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type exp = A of int | B of bool;;" in
  let _, _, _, tylenv, varenv = read_string_eval expr env tyenv tylenv varenv in
  let expr = "let f v = match v with " ^
             "          | A id -> 1 " ^
             "          | B b -> 2;;" in
  let _, env, tyenv, _, _ = read_string_eval expr env tyenv tylenv varenv in
  let id_ty_vals, _, _, _, _ = read_string_eval "f (A 0);;" env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 1)];
  let id_ty_vals, _, _, _, _ = read_string_eval "f (B false);;" env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 2)];
  let expr = "match A 1 with " ^
             "| A id -> 1 " ^
             "| B b -> 2;;" in
  let id_ty_vals, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 1)]

let suite =
  "suite">:::["matching_test1">:: matching_test1;
              "matching_test2">:: matching_test2;
              "matching_test3">:: matching_test3;]

let () = run_test_tt_main suite
