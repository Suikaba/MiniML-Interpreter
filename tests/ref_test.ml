open OUnit2
open Miniml
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let ref_test1 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "let counter = " ^
             "  let r = ref 0 in " ^
             "  let body () = r := !r + 1; !r in " ^
             "  body;;" in
  let _, env, tyenv, _, _, _ = read_string_eval expr env tyenv tylenv varenv [] in
  let id_ty_vals, _, _, _, _, _ = read_string_eval "counter ();;" env tyenv tylenv varenv [] in
  assert_equal id_ty_vals [("-", TyInt, IntV 1)];
  let id_ty_vals, _, _, _, _, _ = read_string_eval "counter ();;" env tyenv tylenv varenv [] in
  assert_equal id_ty_vals [("-", TyInt, IntV 2)];
  let id_ty_vals, _, _, _, _, _ = read_string_eval "counter ();;" env tyenv tylenv varenv [] in
  assert_equal id_ty_vals [("-", TyInt, IntV 3)]

(* weak type variable *)
let ref_test2 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let _, env, tyenv, _, _, _ = read_string_eval "let f = ref (fun x -> x);;" env tyenv tylenv varenv [] in
  let expr = "!f 0;;" in
  let id_ty_vals, env, tyenv, _, _, _ = read_string_eval expr env tyenv tylenv varenv [] in
  assert_equal id_ty_vals [("-", TyInt, IntV 0)];
  try
    let _ = read_string_eval "!f true;;" env tyenv tylenv varenv [] in
    assert_failure "Must be type error";
  with
    Typing.TypeMismatch _ -> ()
  | _ -> assert_failure "Must be type error"

let suite =
  "suite">:::["ref_test1">:: ref_test1;
              "ref_test2">:: ref_test2;]

let () = run_test_tt_main suite

