open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let matching_test1 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "match (1, 2) with
                | (1, 1) -> 1
                | (1, 2) -> 2;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 2)]

let matching_test2 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let rec sum l = match l with
                | [] -> 0
                | hd :: tl -> hd + (sum tl)
              in sum [1; 2; 3; 4; 5];;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 15)]

let suite =
  "suite">:::["matching_test1">:: matching_test1;
              "matching_test2">:: matching_test2;]

let () = run_test_tt_main suite
