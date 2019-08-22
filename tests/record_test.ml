open OUnit2
open Miniml.Syntax
open Miniml.Cui
open Miniml.Eval

let record_test1 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type rec1 = {l:int; r:int;};;" in
  let _, _, _, _, _, recenv = read_string_eval expr env tyenv tylenv varenv [] in
  let expr = "{l = 10; r = 20;};;" in
  let id_ty_vals, _, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv recenv in
  assert_equal id_ty_vals [("-", TyRecord "rec1", RecordV [("l", IntV 10); ("r", IntV 20)])]

let record_test2 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type rec1 = {l:int*int; r:int list;};;" in
  let _, _, _, _, _, recenv = read_string_eval expr env tyenv tylenv varenv [] in
  let expr = "{l = (1, 2); r = [3; 4];};;" in
  let id_ty_vals, _, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv recenv in
  assert_equal id_ty_vals
               [("-", TyRecord "rec1", RecordV [("l", TupleV [IntV 1; IntV 2]); ("r", ListV [IntV 3; IntV 4])])]

let record_test3 _ =
  let env, tyenv, tylenv, varenv = initial_env, initial_tyenv, initial_tyvenv, initial_varenv in
  let expr = "type rec1 = {l:int*int; r:int list;};;" in
  let _, _, _, _, _, recenv = read_string_eval expr env tyenv tylenv varenv [] in
  let expr = "let {l; r;} = {l = (1, 2); r = [3; 4];};;" in
  let id_ty_vals, _, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv recenv in
  assert_equal id_ty_vals
               [("r", TyList TyInt, ListV [IntV 3; IntV 4]);
                ("l", TyTuple [TyInt; TyInt], TupleV [IntV 1; IntV 2]);];
  let expr = "let {l = (a, b); r = c :: d;} = {l = (1, 2); r = [3; 4];};;" in
  let id_ty_vals, _, _, _, _, _ = read_string_eval expr env tyenv tylenv varenv recenv in
  assert_equal id_ty_vals
               [("c", TyInt, IntV 3);
                ("d", TyList TyInt, ListV [IntV 4;]);
                ("a", TyInt, IntV 1);
                ("b", TyInt, IntV 2);]

let suite =
  "suite">:::["record_test1">:: record_test1;
              "record_test2">:: record_test2;
              "record_test3">:: record_test3;]

let () = run_test_tt_main suite
