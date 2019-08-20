open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let if_test1 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let id_ty_vals, _, _ = read_string_eval "if true then 1 else 2;;" env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 1)];
  let id_ty_vals, _, _ = read_string_eval "if false then 1 else 2;;" env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 2)]

let if_test2 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let _, env, tyenv = read_string_eval "let r1 = ref 0 and r2 = ref 0;;" env tyenv in
  let _, env, tyenv = read_string_eval "if true then r1 := !r1 + 1 else r2 := !r2 + 1;;" env tyenv in
  let id_ty_vals, _, _ = read_string_eval "!r1, !r2;;" env tyenv in
  assert_equal id_ty_vals [("-", TyTuple [TyInt; TyInt], TupleV [IntV 1; IntV 0])];
  let _, env, tyenv = read_string_eval "if false then r1 := !r1 + 1 else r2 := !r2 + 1;;" env tyenv in
  let id_ty_vals, _, _ = read_string_eval "!r1, !r2;;" env tyenv in
  assert_equal id_ty_vals [("-", TyTuple [TyInt; TyInt], TupleV [IntV 1; IntV 1])]

let suite =
  "suite">:::["if_test1">:: if_test1;
              "if_test2">:: if_test2;]

let () = run_test_tt_main suite
