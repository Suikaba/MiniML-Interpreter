open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let let_test1 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let x = 10 in let y = 20 in x + y;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 30)]

let let_test2 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let (a, b), [c; d] = (1, 2), [3; 4] in a + b + c + d;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 10)]

let let_test3 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let add x y = x + y in add 1 2;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 3)]

let let_test4 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let add_pair (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) in add_pair (1, 2) (3, 4);;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyTuple [TyInt; TyInt], TupleV [IntV 4; IntV 6])]

(* polymorphism *)
let let_poly_test1 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let apply_three_times f x = f (f (f x));;" in
  let _, env, tyenv = read_string_eval expr env tyenv in
  let id_ty_vals, _, _ = read_string_eval "apply_three_times (fun x -> x * 2) 2;;" env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 16)];
  let id_ty_vals, _, _ = read_string_eval "apply_three_times not true;;" env tyenv in
  assert_equal id_ty_vals [("-", TyBool, BoolV false)]

let suite =
  "suite">:::["let_test1">:: let_test1;
              "let_test2">:: let_test2;
              "let_test3">:: let_test3;
              "let_test4">:: let_test4;
              "let_poly_test1">:: let_poly_test1;]

let () = run_test_tt_main suite
