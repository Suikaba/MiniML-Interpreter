open OUnit2
open Miniml
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let let_rec_test1 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let rec fact n = " ^
             "  if n < 1 then 1 else n * fact (n - 1)" ^
             "in fact 5;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 120)]

let let_rec_test2 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let rec even n = " ^
             "  if n = 0 then true else odd (n - 1)" ^
             "and odd n =" ^
             "  if n = 0 then false else even (n - 1);;" in
  let _, env, tyenv = read_string_eval expr env tyenv in
  let expr = "even 10, even 11, odd 10, odd 11;;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyTuple [TyBool; TyBool; TyBool; TyBool],
                            TupleV [BoolV true; BoolV false; BoolV false; BoolV true])]

let let_rec_test3 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let rec f x = x " ^
             "and y = 10 " ^
             "and mult2 x = x * 2 " ^
             "in mult2 (f y);;" in
  let id_ty_vals, _, _ = read_string_eval expr env tyenv in
  assert_equal id_ty_vals [("-", TyInt, IntV 20)]

let let_rec_test4 _ =
  let env, tyenv = initial_env, initial_tyenv in
  let expr = "let rec f x = x and g = f in g 10;;" in
  try
    let _ = read_string_eval expr env tyenv in
    assert_failure "never reached"
  with
    Typing.Error _ -> ()
  | _ -> assert_failure "error"

let suite =
  "suite">:::["let_rec_test1">:: let_rec_test1;
              "let_rec_test2">:: let_rec_test2;
              "let_rec_test3">:: let_rec_test3;
              "let_rec_test4">:: let_rec_test4;]

let () = run_test_tt_main suite
